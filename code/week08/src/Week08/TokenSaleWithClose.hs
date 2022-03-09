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
    , TSStartSchema
    , TSUseSchema
    , startEndpoint
    , useEndpoints
    , useEndpoints'
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value
import           Prelude                      (Semigroup (..), Show (..))
import qualified Prelude

data TokenSale = TokenSale
    { tsSeller :: !PaymentPubKeyHash
    , tsToken  :: !AssetClass
    , tsTT     :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

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
    (v, Just _, SetPrice p)   | p >= 0                             -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                                           , State (Just p) v
                                                                           )
    (v, Just p, AddTokens n)  | n > 0                              -> Just ( mempty
                                                                           , State (Just p) $
                                                                             v                                       <>
                                                                             assetClassValue (tsToken ts) n
                                                                           )
    (v, Just p, BuyTokens n)  | n > 0                              -> Just ( mempty
                                                                           , State (Just p) $
                                                                             v                                       <>
                                                                             assetClassValue (tsToken ts) (negate n) <>
                                                                             lovelaceValueOf (n * p)
                                                                           )
    (v, Just p, Withdraw n l) | n >= 0 && l >= 0 &&
                                v `geq` (w <> toValue minAdaTxOut) -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                                           , State (Just p) $
                                                                             v                                       <>
                                                                             negate w
                                                                           )
      where
        w = assetClassValue (tsToken ts) n <>
            lovelaceValueOf l
    (_, Just _, Close)                                             -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                                           , State Nothing mempty
                                                                           )
    _                                                              -> Nothing

{-# INLINABLE tsStateMachine #-}
tsStateMachine :: TokenSale -> StateMachine (Maybe Integer) TSRedeemer
tsStateMachine ts = mkStateMachine (Just $ tsTT ts) (transition ts) isNothing

{-# INLINABLE mkTSValidator #-}
mkTSValidator :: TokenSale -> Maybe Integer -> TSRedeemer -> ScriptContext -> Bool
mkTSValidator = mkValidator . tsStateMachine

type TS = StateMachine (Maybe Integer) TSRedeemer

tsTypedValidator :: TokenSale -> Scripts.TypedValidator TS
tsTypedValidator ts = Scripts.mkTypedValidator @TS
    ($$(PlutusTx.compile [|| mkTSValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode ts)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @(Maybe Integer) @TSRedeemer

tsValidator :: TokenSale -> Validator
tsValidator = Scripts.validatorScript . tsTypedValidator

tsAddress :: TokenSale -> Ledger.Address
tsAddress = scriptAddress . tsValidator

tsClient :: TokenSale -> StateMachineClient (Maybe Integer) TSRedeemer
tsClient ts = mkStateMachineClient $ StateMachineInstance (tsStateMachine ts) (tsTypedValidator ts)

mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
mapErrorSM = mapError $ pack . show

startTS :: AssetClass -> Contract (Last TokenSale) s Text ()
startTS token = do
    pkh <- Contract.ownPaymentPubKeyHash
    tt  <- mapErrorSM getThreadToken
    let ts = TokenSale
            { tsSeller = pkh
            , tsToken  = token
            , tsTT     = tt
            }
        client = tsClient ts
    void $ mapErrorSM $ runInitialise client (Just 0) mempty
    tell $ Last $ Just ts
    logInfo $ "started token sale " ++ show ts

setPrice :: TokenSale -> Integer -> Contract w s Text ()
setPrice ts p = void $ mapErrorSM $ runStep (tsClient ts) $ SetPrice p

addTokens :: TokenSale -> Integer -> Contract w s Text ()
addTokens ts n = void (mapErrorSM $ runStep (tsClient ts) $ AddTokens n)

buyTokens :: TokenSale -> Integer -> Contract w s Text ()
buyTokens ts n = void $ mapErrorSM $ runStep (tsClient ts) $ BuyTokens n

withdraw :: TokenSale -> Integer -> Integer -> Contract w s Text ()
withdraw ts n l = void $ mapErrorSM $ runStep (tsClient ts) $ Withdraw n l

close :: TokenSale -> Contract w s Text ()
close ts = void $ mapErrorSM $ runStep (tsClient ts) Close

type TSStartSchema =
        Endpoint "start"      (CurrencySymbol, TokenName)
type TSUseSchema =
        Endpoint "set price"  Integer
    .\/ Endpoint "add tokens" Integer
    .\/ Endpoint "buy tokens" Integer
    .\/ Endpoint "withdraw"   (Integer, Integer)
    .\/ Endpoint "close"      ()

startEndpoint :: Contract (Last TokenSale) TSStartSchema Text ()
startEndpoint = forever
              $ handleError logError
              $ awaitPromise
              $ endpoint @"start" $ startTS . AssetClass

useEndpoints' :: ( HasEndpoint "set price" Integer s
                 , HasEndpoint "add tokens" Integer s
                 , HasEndpoint "buy tokens" Integer s
                 , HasEndpoint "withdraw" (Integer, Integer) s
                 , HasEndpoint "close" () s
                 )
              => TokenSale
              -> Promise () s Text ()
useEndpoints' ts = setPrice' `select` addTokens' `select` buyTokens' `select` withdraw' `select` close'
  where
    setPrice'  = endpoint @"set price"  $ \p      -> handleError logError (setPrice ts p)
    addTokens' = endpoint @"add tokens" $ \n      -> handleError logError (addTokens ts n)
    buyTokens' = endpoint @"buy tokens" $ \n      -> handleError logError (buyTokens ts n)
    withdraw'  = endpoint @"withdraw"   $ \(n, l) -> handleError logError (withdraw ts n l)
    close'     = endpoint @"close"      $ \()     -> handleError logError (close ts)

useEndpoints :: TokenSale -> Contract () TSUseSchema Text ()
useEndpoints = forever . awaitPromise . useEndpoints'
