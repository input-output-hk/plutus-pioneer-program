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

module Week08.TokenSaleWithClose
    ( TokenSale (..)
    , TSRedeemer (..)
    , nftName
    , TSStartSchema
    , TSStartSchema'
    , TSUseSchema
    , startEndpoint
    , startEndpoint'
    , useEndpoints
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
import           Prelude                      (Semigroup (..), Show (..), uncurry)
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
    | Close
    deriving (Show, Prelude.Eq)

PlutusTx.unstableMakeIsData ''TSRedeemer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE transition #-}
transition :: TokenSale -> State (Maybe Integer) -> TSRedeemer -> Maybe (TxConstraints Void Void, State (Maybe Integer))
transition ts s r = case (stateValue s, stateData s, r) of
    (v, Just _, SetPrice p)   | p >= 0           -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                         , State (Just p) $
                                                           v <>
                                                           nft (negate 1)
                                                         )
    (v, Just p, AddTokens n)  | n > 0            -> Just ( mempty
                                                         , State (Just p) $
                                                           v                                       <>
                                                           nft (negate 1)                          <>
                                                           assetClassValue (tsToken ts) n
                                                         )
    (v, Just p, BuyTokens n)  | n > 0            -> Just ( mempty
                                                         , State (Just p) $
                                                           v                                       <>
                                                           nft (negate 1)                          <>
                                                           assetClassValue (tsToken ts) (negate n) <>
                                                           lovelaceValueOf (n * p)
                                                         )
    (v, Just p, Withdraw n l) | n >= 0 && l >= 0 -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                         , State (Just p) $
                                                           v                                       <>
                                                           nft (negate 1)                          <>
                                                           assetClassValue (tsToken ts) (negate n) <>
                                                           lovelaceValueOf (negate l)
                                                         )
    (_, Just _, Close)                           -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                         , State Nothing $
                                                           mempty
                                                         )
    _                                            -> Nothing
  where
    nft :: Integer -> Value
    nft = assetClassValue (tsNFT ts)

{-# INLINABLE tsStateMachine #-}
tsStateMachine :: TokenSale -> StateMachine (Maybe Integer) TSRedeemer
tsStateMachine ts = mkStateMachine (Just $ tsNFT ts) (transition ts) isNothing

{-# INLINABLE mkTSValidator #-}
mkTSValidator :: TokenSale -> Maybe Integer -> TSRedeemer -> ScriptContext -> Bool
mkTSValidator = mkValidator . tsStateMachine

type TS = StateMachine (Maybe Integer) TSRedeemer

tsInst :: TokenSale -> Scripts.ScriptInstance TS
tsInst ts = Scripts.validator @TS
    ($$(PlutusTx.compile [|| mkTSValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode ts)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @(Maybe Integer) @TSRedeemer

tsValidator :: TokenSale -> Validator
tsValidator = Scripts.validatorScript . tsInst

tsAddress :: TokenSale -> Ledger.Address
tsAddress = scriptAddress . tsValidator

tsClient :: TokenSale -> StateMachineClient (Maybe Integer) TSRedeemer
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
    void $ mapErrorSM $ runInitialise client (Just 0) mempty
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

close :: HasBlockchainActions s => TokenSale -> Contract w s Text ()
close ts = void $ mapErrorSM $ runStep (tsClient ts) Close

type TSStartSchema = BlockchainActions
    .\/ Endpoint "start"      (CurrencySymbol, TokenName)
type TSStartSchema' = BlockchainActions
    .\/ Endpoint "start"      (CurrencySymbol, CurrencySymbol, TokenName)
type TSUseSchema = BlockchainActions
    .\/ Endpoint "set price"  Integer
    .\/ Endpoint "add tokens" Integer
    .\/ Endpoint "buy tokens" Integer
    .\/ Endpoint "withdraw"   (Integer, Integer)
    .\/ Endpoint "close"      ()

startEndpoint :: Contract (Last TokenSale) TSStartSchema Text ()
startEndpoint = startTS' >> startEndpoint
  where
    startTS' = handleError logError $ endpoint @"start"  >>= void . startTS Nothing . AssetClass

startEndpoint' :: Contract (Last TokenSale) TSStartSchema' Text ()
startEndpoint' = startTS' >> startEndpoint'
  where
    startTS' = handleError logError $ endpoint @"start"  >>= \(cs1, cs2, tn) ->  void $ startTS (Just cs1) $ AssetClass (cs2, tn)

useEndpoints :: TokenSale -> Contract () TSUseSchema Text ()
useEndpoints ts = (setPrice' `select` addTokens' `select` buyTokens' `select` withdraw' `select` close') >> useEndpoints ts
  where
    setPrice'  = handleError logError $ endpoint @"set price"  >>= setPrice ts
    addTokens' = handleError logError $ endpoint @"add tokens" >>= addTokens ts
    buyTokens' = handleError logError $ endpoint @"buy tokens" >>= buyTokens ts
    withdraw'  = handleError logError $ endpoint @"withdraw"   >>= uncurry (withdraw ts)
    close'     = handleError logError $ endpoint @"close"      >>  close ts
