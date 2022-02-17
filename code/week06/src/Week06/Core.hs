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

module Week06.Core
    ( Oracle (..)
    , OracleRedeemer (..)
    , oracleTokenName
    , oracleValue
    , oracleAsset
    , typedOracleValidator
    , oracleValidator
    , oracleAddress
    , OracleSchema
    , OracleParams (..)
    , runOracle
    , findOracle
    ) where

import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.OpenApi.Schema       (ToSchema)
import           Data.Text                 (Text, pack)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Plutus.Contracts.Currency as Currency
import           Prelude                   (Semigroup (..), Show (..), String)
import qualified Prelude

data Oracle = Oracle
    { oSymbol   :: !CurrencySymbol
    , oOperator :: !PaymentPubKeyHash
    , oFee      :: !Integer
    , oAsset    :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord, ToSchema)

PlutusTx.makeLift ''Oracle

data OracleRedeemer = Update | Use
    deriving Show

PlutusTx.unstableMakeIsData ''OracleRedeemer

{-# INLINABLE oracleTokenName #-}
oracleTokenName :: TokenName
oracleTokenName = TokenName emptyByteString

{-# INLINABLE oracleAsset #-}
oracleAsset :: Oracle -> AssetClass
oracleAsset oracle = AssetClass (oSymbol oracle, oracleTokenName)

{-# INLINABLE oracleValue #-}
oracleValue :: Maybe Datum -> Maybe Integer
oracleValue md = do
    Datum d <- md
    PlutusTx.fromBuiltinData d

{-# INLINABLE mkOracleValidator #-}
mkOracleValidator :: Oracle -> Integer -> OracleRedeemer -> ScriptContext -> Bool
mkOracleValidator oracle x r ctx =
    traceIfFalse "token missing from input"  inputHasToken  &&
    traceIfFalse "token missing from output" outputHasToken &&
    case r of
        Update -> traceIfFalse "operator signature missing" (txSignedBy info $ unPaymentPubKeyHash $ oOperator oracle) &&
                  traceIfFalse "invalid output datum"       validOutputDatum
        Use    -> traceIfFalse "oracle value changed"       (outputDatum == Just x)              &&
                  traceIfFalse "fees not paid"              feesPaid
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i

    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oracleAsset oracle) == 1

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle output"

    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleAsset oracle) == 1

    outputDatum :: Maybe Integer
    outputDatum = oracleValue $ txOutDatum ownOutput >>= flip findDatum info

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatum

    feesPaid :: Bool
    feesPaid =
      let
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
      in
        outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))

data Oracling
instance Scripts.ValidatorTypes Oracling where
    type instance DatumType Oracling = Integer
    type instance RedeemerType Oracling = OracleRedeemer

typedOracleValidator :: Oracle -> Scripts.TypedValidator Oracling
typedOracleValidator oracle = Scripts.mkTypedValidator @Oracling
    ($$(PlutusTx.compile [|| mkOracleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @OracleRedeemer

oracleValidator :: Oracle -> Validator
oracleValidator = Scripts.validatorScript . typedOracleValidator

oracleAddress :: Oracle -> Ledger.Address
oracleAddress = scriptAddress . oracleValidator

data OracleParams = OracleParams
    { opFees   :: !Integer
    , opSymbol :: !CurrencySymbol
    , opToken  :: !TokenName
    } deriving (Show, Prelude.Eq, Prelude.Ord, Generic, FromJSON, ToJSON, ToSchema)

startOracle :: forall w s. OracleParams -> Contract w s Text Oracle
startOracle op = do
    pkh <- Contract.ownPaymentPubKeyHash
    osc <- mapError (pack . show) (mintContract pkh [(oracleTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency)
    let cs     = Currency.currencySymbol osc
        oracle = Oracle
            { oSymbol   = cs
            , oOperator = pkh
            , oFee      = opFees op
            , oAsset    = AssetClass (opSymbol op, opToken op)
            }
    logInfo @String $ "started oracle " ++ show oracle
    return oracle

updateOracle :: forall w s. Oracle -> Integer -> Contract w s Text ()
updateOracle oracle x = do
    m <- findOracle oracle
    let c = Constraints.mustPayToTheScript x $ assetClassValue (oracleAsset oracle) 1 <> lovelaceValueOf 2_000_000
    case m of
        Nothing -> do
            ledgerTx <- submitTxConstraints (typedOracleValidator oracle) c
            awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "set initial oracle value to " ++ show x
        Just (oref, o,  _) -> do
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>
                          Constraints.typedValidatorLookups (typedOracleValidator oracle) <>
                          Constraints.otherScript (oracleValidator oracle)
                tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Update)
            ledgerTx <- submitTxConstraintsWith @Oracling lookups tx
            awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "updated oracle value to " ++ show x

findOracle :: forall w s. Oracle -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, Integer))
findOracle oracle = do
    utxos <- Map.filter f <$> utxosAt (oracleAddress oracle)
    return $ case Map.toList utxos of
        [(oref, o)] -> do
            let md = either (const Nothing) Just $ _ciTxOutDatum o
            x <- oracleValue md
            return (oref, o, x)
        _           -> Nothing
  where
    f :: ChainIndexTxOut -> Bool
    f o = assetClassValueOf (_ciTxOutValue o) (oracleAsset oracle) == 1

type OracleSchema = Endpoint "update" Integer

runOracle :: OracleParams -> Contract (Last Oracle) OracleSchema Text ()
runOracle op = do
    oracle <- startOracle op
    tell $ Last $ Just oracle
    go oracle
  where
    go :: Oracle -> Contract (Last Oracle) OracleSchema Text a
    go oracle = do
        x <- awaitPromise $ endpoint @"update" return
        updateOracle oracle x
        go oracle
