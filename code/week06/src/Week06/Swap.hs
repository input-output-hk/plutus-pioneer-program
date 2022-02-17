{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week06.Swap
    ( SwapSchema
    , swap
    ) where

import           Control.Monad        hiding (fmap)
import           Data.List            (find)
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import           Data.Monoid          (Last (..))
import           Data.Text            (Text)
import           Plutus.Contract      as Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), (<$>), unless, mapMaybe, find)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada hiding (divide)
import           Ledger.Value         as Value
import           Prelude              (Semigroup (..), Show (..), String)

import           Week06.Core
import           Week06.Funds

{-# INLINABLE price #-}
price :: Integer -> Integer -> Integer
price lovelace exchangeRate = (lovelace * exchangeRate) `divide` 1000000

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE mkSwapValidator #-}
mkSwapValidator :: Oracle -> Address -> PaymentPubKeyHash -> () -> ScriptContext -> Bool
mkSwapValidator oracle addr pkh () ctx =
    txSignedBy info (unPaymentPubKeyHash pkh) ||
    (traceIfFalse "expected exactly two script inputs" hasTwoScriptInputs &&
     traceIfFalse "price not paid"                     sellerPaid)

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    oracleInput :: TxOut
    oracleInput =
      let
        ins = [ o
              | i <- txInfoInputs info
              , let o = txInInfoResolved i
              , txOutAddress o == addr
              ]
      in
        case ins of
            [o] -> o
            _   -> traceError "expected exactly one oracle input"

    oracleValue' = case oracleValue (txOutDatumHash oracleInput >>= flip findDatum info) of
        Nothing -> traceError "oracle value not found"
        Just x  -> x

    hasTwoScriptInputs :: Bool
    hasTwoScriptInputs =
      let
        xs = filter (isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
      in
        length xs == 2

    minPrice :: Integer
    minPrice =
      let
        lovelaceIn = case findOwnInput ctx of
            Nothing -> traceError "own input not found"
            Just i  -> lovelaces $ txOutValue $ txInInfoResolved i
      in
        price lovelaceIn oracleValue'

    sellerPaid :: Bool
    sellerPaid =
      let
        pricePaid :: Integer
        pricePaid =  assetClassValueOf (valuePaidTo info $ unPaymentPubKeyHash pkh) (oAsset oracle)
      in
        pricePaid >= minPrice

data Swapping
instance Scripts.ValidatorTypes Swapping where
    type instance DatumType Swapping = PaymentPubKeyHash
    type instance RedeemerType Swapping = ()

typedSwapValidator :: Oracle -> Scripts.TypedValidator Swapping
typedSwapValidator oracle = Scripts.mkTypedValidator @Swapping
    ($$(PlutusTx.compile [|| mkSwapValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode oracle
        `PlutusTx.applyCode` PlutusTx.liftCode (oracleAddress oracle))
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PaymentPubKeyHash @()

swapValidator :: Oracle -> Validator
swapValidator = Scripts.validatorScript . typedSwapValidator

swapAddress :: Oracle -> Ledger.Address
swapAddress = scriptAddress . swapValidator

offerSwap :: forall w s. Oracle -> Integer -> Contract w s Text ()
offerSwap oracle amt = do
    pkh <- Contract.ownPaymentPubKeyHash
    let tx = Constraints.mustPayToTheScript pkh $ Ada.lovelaceValueOf amt
    ledgerTx <- submitTxConstraints (typedSwapValidator oracle) tx
    awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "offered " ++ show amt ++ " lovelace for swap"

findSwaps :: Oracle -> (PaymentPubKeyHash -> Bool) -> Contract w s Text [(TxOutRef, ChainIndexTxOut, PaymentPubKeyHash)]
findSwaps oracle p = do
    utxos <- utxosAt $ swapAddress oracle
    return $ mapMaybe g $ Map.toList utxos
  where
    f :: ChainIndexTxOut -> Maybe PaymentPubKeyHash
    f o = do
        (Datum d) <- either (const Nothing) Just $ _ciTxOutDatum o
        PlutusTx.fromBuiltinData d

    g :: (TxOutRef, ChainIndexTxOut) -> Maybe (TxOutRef, ChainIndexTxOut, PaymentPubKeyHash)
    g (oref, o) = do
        pkh <- f o
        guard $ p pkh
        return (oref, o, pkh)

retrieveSwaps :: Oracle -> Contract w s Text ()
retrieveSwaps oracle = do
    pkh <- Contract.ownPaymentPubKeyHash
    xs  <- findSwaps oracle (== pkh)
    case xs of
        [] -> logInfo @String "no swaps found"
        _  -> do
            let lookups = Constraints.unspentOutputs (Map.fromList [(oref, o) | (oref, o, _) <- xs]) <>
                          Constraints.otherScript (swapValidator oracle)
                tx      = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData () | (oref, _, _) <- xs]
            ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
            awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "retrieved " ++ show (length xs) ++ " swap(s)"

useSwap :: forall w s. Oracle -> Address -> Contract w s Text ()
useSwap oracle addr = do
    v <- funds addr
    let amt = assetClassValueOf v $ oAsset oracle
    logInfo @String $ "available assets: " ++ show amt

    m <- findOracle oracle
    case m of
        Nothing           -> logInfo @String "oracle not found"
        Just (oref, o, x) -> do
            logInfo @String $ "found oracle, exchange rate " ++ show x
            pkh   <- Contract.ownPaymentPubKeyHash
            swaps <- findSwaps oracle (/= pkh)
            case find (f amt x) swaps of
                Nothing                -> logInfo @String "no suitable swap found"
                Just (oref', o', pkh') -> do
                    let w       = _ciTxOutValue o <> lovelaceValueOf (oFee oracle)
                        p       = assetClassValue (oAsset oracle) (price (lovelaces $ _ciTxOutValue  o') x) <>
                                  lovelaceValueOf 2_000_000
                        lookups = Constraints.otherScript (swapValidator oracle)                     <>
                                  Constraints.otherScript (oracleValidator oracle)                   <>
                                  Constraints.unspentOutputs (Map.fromList [(oref, o), (oref', o')])
                        tx      = Constraints.mustSpendScriptOutput oref  (Redeemer $ PlutusTx.toBuiltinData Use) <>
                                  Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData ())  <>
                                  Constraints.mustPayToOtherScript
                                    (validatorHash $ oracleValidator oracle)
                                    (Datum $ PlutusTx.toBuiltinData x)
                                    w                                                                             <>
                                  Constraints.mustPayToPubKey pkh' p
                    ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
                    awaitTxConfirmed $ getCardanoTxId ledgerTx
                    logInfo @String $ "made swap with price " ++ show (Value.flattenValue p)
  where
    getPrice :: Integer -> ChainIndexTxOut -> Integer
    getPrice x o = price (lovelaces $ _ciTxOutValue o) x

    f :: Integer -> Integer -> (TxOutRef, ChainIndexTxOut, PaymentPubKeyHash) -> Bool
    f amt x (_, o, _) = getPrice x o <= amt

type SwapSchema =
            Endpoint "offer"    Integer
        .\/ Endpoint "retrieve" ()
        .\/ Endpoint "use"      ()
        .\/ Endpoint "funds"    ()

swap :: Oracle -> Address -> Contract (Last Value) SwapSchema Text ()
swap oracle addr = awaitPromise (offer `select` retrieve `select` use `select` funds'') >> swap oracle addr
  where
    offer :: Promise (Last Value) SwapSchema Text ()
    offer = endpoint @"offer" $ \amt -> h $ offerSwap oracle amt

    retrieve :: Promise (Last Value) SwapSchema Text ()
    retrieve =  endpoint @"retrieve" $ \() -> h $ retrieveSwaps oracle

    use :: Promise (Last Value) SwapSchema Text ()
    use = endpoint @"use" $ \() -> h $ useSwap oracle addr

    funds'' :: Promise (Last Value) SwapSchema Text ()
    funds'' = endpoint @"funds" $ \() -> h $ funds addr >>= tell . Last . Just

    h :: Contract (Last Value) SwapSchema Text () -> Contract (Last Value) SwapSchema Text ()
    h = handleError logError
