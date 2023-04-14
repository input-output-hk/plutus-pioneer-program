{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings  #-}

module Oracle where

import           GHC.Generics                   (Generic)
import Plutus.V2.Ledger.Api
    ( BuiltinData,
      ScriptContext(scriptContextTxInfo),
      mkValidatorScript,
      PubKeyHash,
      Datum(Datum),
      DatumHash,
      Validator,
      TxInInfo(txInInfoResolved),
      TxInfo,
      OutputDatum(OutputDatumHash, NoOutputDatum, OutputDatum),
      TxOut(txOutDatum, txOutValue) )
import qualified Plutus.V2.Ledger.Contexts      as V2
import qualified PlutusTx
import PlutusTx.Prelude
    ( Bool,
      Integer,
      Maybe(..),
      ($),
      (.),
      (&&),
      tail,
      isJust,
      traceError,
      traceIfFalse,
      Eq(..),
      Ord, 
      take 
      )
import           Prelude                    (Show (show), span, IO)
import qualified  Prelude               (Eq, (/=),Ord )
import Data.String ( IsString(fromString), String )
import Plutus.V1.Ledger.Value
    ( assetClassValueOf, AssetClass(AssetClass) )
import           Utilities            (wrapValidator, writeValidatorToFile)
import Text.Printf (printf)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- It provides the price of the Collateral coin in ada. In the Datum.

-- TODO: Update to use oracle as reference input
data OracleParams = OracleParams
    { oAssetClass :: AssetClass
    , oOperator   :: PubKeyHash
    } deriving (Show, Generic, Eq, Ord)

PlutusTx.makeLift ''OracleParams

data OracleRedeemer = Use | Update | Redeem
    deriving Prelude.Show

PlutusTx.unstableMakeIsData ''OracleRedeemer

type Ratio = Integer

{-# INLINABLE oracleValue #-}
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer
oracleValue o f = case txOutDatum o of
    NoOutputDatum -> Nothing
    OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d
    OutputDatumHash dh -> do
                        Datum d <- f dh
                        PlutusTx.fromBuiltinData d

{-# INLINABLE mkValidator #-}
mkValidator :: OracleParams -> Ratio -> OracleRedeemer -> ScriptContext -> Bool
mkValidator oracle x r ctx =
    case r of
        Use    -> traceIfFalse "token missing from input"  inputHasToken  &&
                  traceIfFalse "token missing from output" outputHasToken &&
                  traceIfFalse "oracle value changed"       (outputDatum == Just x)
        Update -> traceIfFalse "token missing from input"  inputHasToken  &&
                  traceIfFalse "token missing from output" outputHasToken &&
                  traceIfFalse "operator signature missing" checkOperatorSignature &&
                  traceIfFalse "invalid output datum"       validOutputDatum
        Redeem -> traceIfFalse "operator signature missing" checkOperatorSignature

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkOperatorSignature :: Bool
    checkOperatorSignature = V2.txSignedBy info $ oOperator oracle

    ownInput :: TxOut
    ownInput = case V2.findOwnInput ctx of
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i

    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oAssetClass oracle) == 1

    ownOutput :: TxOut
    ownOutput = case V2.getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle output"

    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oAssetClass oracle) == 1

    outputDatum :: Maybe Integer
    outputDatum = oracleValue ownOutput (`V2.findDatum` info)

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatum

{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: OracleParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator . mkValidator

validator :: OracleParams -> Validator
validator oracle = mkValidatorScript $
    $$(PlutusTx.compile [|| mkWrappedValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oracle


---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveOracleScript :: String -> PubKeyHash -> IO ()
saveOracleScript symbol pkh = do
    let
    writeValidatorToFile fp $ validator op
    where
        op = OracleParams
            { oAssetClass = parseToken symbol
            , oOperator   = pkh
            }
        fp = printf "assets/oracle-%s-%s.plutus" (take 3 (show pkh)) $ take 3 (show pkh)

parseToken :: String -> AssetClass
parseToken s =
  let
    (x, y) = span (Prelude./= '.') s
  in
    AssetClass (fromString x, fromString $ tail y)