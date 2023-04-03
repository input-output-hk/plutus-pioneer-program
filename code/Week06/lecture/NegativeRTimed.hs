{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

module NegativeRTimed where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx             (compile, unstableMakeIsData)
import PlutusTx.Builtins ( Integer, BuiltinData )
import PlutusTx.Prelude ( Bool, ($), traceIfFalse, Ord((<=)), (&&) ) 
import Plutus.V2.Ledger.Api (ScriptContext (scriptContextTxInfo), POSIXTime, TxInfo (txInfoValidRange), from)
import Utilities (wrapValidator)
import Plutus.V1.Ledger.Interval (contains)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

newtype CustomDatum = MkCustomDatum { deadline :: POSIXTime }
unstableMakeIsData ''CustomDatum

{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatum -> Integer -> ScriptContext -> Bool
mkValidator (MkCustomDatum d) r ctx = traceIfFalse "expected a negative redeemer" $ r <= 0 &&
                                      traceIfFalse "deadline not reached" deadlineReached
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        deadlineReached :: Bool
        deadlineReached = contains (from d) $ txInfoValidRange info


{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator mkValidator

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mkWrappedValidator ||])