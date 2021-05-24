{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week08.TokenSale
    ( TokenSale (..)
    , TSRedeemer (..)
    , nftName
    , TSOperateSchema
    , TSOperateSchema'
    , TSUseSchema
    , operateTS'
    , operateTS''
    , useTS
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Plutus.Contract              as Contract hiding (when)
import           Plutus.Contract.StateMachine
import qualified Plutus.Contracts.Currency    as C
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value
import           Prelude                      (Semigroup (..))
import qualified Prelude

data TokenSale = TokenSale
    { tsSeller :: !PubKeyHash
    , tsToken  :: !AssetClass
    , tsNFT    :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''TokenSale

data TSRedeemer =
      SetPrice Integer
    | AddTokens Integer
    | BuyTokens Integer
    | Withdraw Integer Integer
    deriving (Show, Prelude.Eq)

PlutusTx.unstableMakeIsData ''TSRedeemer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE transition #-}
transition :: TokenSale -> State Integer -> TSRedeemer -> Maybe (TxConstraints Void Void, State Integer)
transition ts s r = case (stateValue s, stateData s, r) of
    (v, _, SetPrice p)   | p >= 0           -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                    , State p (v <> nft (negate 1))
                                                    )
    (v, p, AddTokens n)  | n > 0            -> Just ( mempty
                                                    , State p $ v <> nft (negate 1) <> assetClassValue (tsToken ts) n
                                                    )
    (v, p, BuyTokens n)  | n > 0            -> Just ( mempty
                                                    , State p $ v <> nft (negate 1) <> assetClassValue (tsToken ts) (negate n) <> lovelaceValueOf (n * p)
                                                    )
    (v, p, Withdraw n l) | n >= 0 && l >= 0 -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                    , State p $ v <> nft (negate 1) <> assetClassValue (tsToken ts) (negate n) <> lovelaceValueOf (negate l)
                                                    )
    _                                       -> Nothing
  where
    nft :: Integer -> Value
    nft = assetClassValue (tsNFT ts)

{-# INLINABLE tsStateMachine #-}
tsStateMachine :: TokenSale -> StateMachine Integer TSRedeemer
tsStateMachine ts = mkStateMachine (Just $ tsNFT ts) (transition ts) (const False)

{-# INLINABLE mkTSValidator #-}
mkTSValidator :: TokenSale -> Integer -> TSRedeemer -> ScriptContext -> Bool
mkTSValidator = mkValidator . tsStateMachine

type TS = StateMachine Integer TSRedeemer

tsInst :: TokenSale -> Scripts.ScriptInstance TS
tsInst ts = Scripts.validator @TS
    ($$(PlutusTx.compile [|| mkTSValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode ts)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @TSRedeemer

tsValidator :: TokenSale -> Validator
tsValidator = Scripts.validatorScript . tsInst

tsAddress :: TokenSale -> Ledger.Address
tsAddress = scriptAddress . tsValidator

tsClient :: TokenSale -> StateMachineClient Integer TSRedeemer
tsClient ts = mkStateMachineClient $ StateMachineInstance (tsStateMachine ts) (tsInst ts)

mapErrorC :: Contract w s C.CurrencyError a -> Contract w s Text a
mapErrorC = mapError $ pack . show

mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
mapErrorSM = mapError $ pack . show


nftName :: TokenName
nftName = "NFT"

startTS :: HasBlockchainActions s => Maybe CurrencySymbol -> AssetClass -> Contract (Last TokenSale) s Text TokenSale
startTS mcs token = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    cs  <- case mcs of
        Nothing  -> C.currencySymbol <$> mapErrorC (C.forgeContract pkh [(nftName, 1)])
        Just cs' -> return cs'
    let ts = TokenSale
            { tsSeller = pkh
            , tsToken  = token
            , tsNFT    = AssetClass (cs, nftName)
            }
        client = tsClient ts
    void $ mapErrorSM $ runInitialise client 0 mempty
    tell $ Last $ Just ts
    logInfo $ "started token sale " ++ show ts
    return ts

setPrice :: HasBlockchainActions s => TokenSale -> Integer -> Contract w s Text ()
setPrice ts p = void $ mapErrorSM $ runStep (tsClient ts) $ SetPrice p

addTokens :: HasBlockchainActions s => TokenSale -> Integer -> Contract w s Text ()
addTokens ts n = void (mapErrorSM $ runStep (tsClient ts) $ AddTokens n)

buyTokens :: HasBlockchainActions s => TokenSale -> Integer -> Contract w s Text ()
buyTokens ts n = void $ mapErrorSM $ runStep (tsClient ts) $ BuyTokens n

withdraw :: HasBlockchainActions s => TokenSale -> Integer -> Integer -> Contract w s Text ()
withdraw ts n l = void $ mapErrorSM $ runStep (tsClient ts) $ Withdraw n l

type TSOperateSchema  = BlockchainActions
    .\/ Endpoint "start"      (CurrencySymbol, TokenName)
    .\/ Endpoint "set price"  Integer
    .\/ Endpoint "add tokens" Integer
    .\/ Endpoint "withdraw"   (Integer, Integer)
type TSOperateSchema' = BlockchainActions
    .\/ Endpoint "start" (CurrencySymbol, CurrencySymbol, TokenName)
    .\/ Endpoint "set price"  Integer
    .\/ Endpoint "add tokens" Integer
    .\/ Endpoint "withdraw"   (Integer, Integer)
type TSUseSchema    = BlockchainActions .\/ Endpoint "buy tokens" Integer

operateTS :: forall s.
             ( HasBlockchainActions                        s
             , HasEndpoint "set price"  Integer            s
             , HasEndpoint "add tokens" Integer            s
             , HasEndpoint "withdraw"   (Integer, Integer) s
             )
          => Maybe CurrencySymbol
          -> CurrencySymbol
          -> TokenName
          -> Contract (Last TokenSale) s Text ()
operateTS mcs cs tn = startTS mcs (AssetClass (cs, tn)) >>= go
  where
    go :: TokenSale -> Contract (Last TokenSale) s Text ()
    go ts = (setPrice' `select` addTokens' `select` withdraw') >> go ts
      where
        setPrice'  = handleError logError $ endpoint @"set price"  >>= setPrice ts
        addTokens' = handleError logError $ endpoint @"add tokens" >>= addTokens ts
        withdraw'  = handleError logError $ endpoint @"withdraw"   >>= uncurry (withdraw ts)

operateTS' :: Contract (Last TokenSale) TSOperateSchema Text ()
operateTS' = endpoint @"start" >>= uncurry (operateTS Nothing)

operateTS'' :: Contract (Last TokenSale) TSOperateSchema' Text ()
operateTS'' = endpoint @"start" >>= \(cs1, cs2, tn) -> operateTS (Just cs1) cs2 tn

useTS :: TokenSale -> Contract () TSUseSchema Text ()
useTS ts = buyTokens' >> useTS ts
  where
    buyTokens' = handleError logError $ endpoint @"buy tokens" >>= buyTokens ts
