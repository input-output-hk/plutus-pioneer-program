{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

module CustomTypes where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx             (BuiltinData, compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool, Eq ((==)), Integer, traceIfFalse,
                                       ($))
import           Prelude              (IO)
import           Utilities            (wrapValidator, writeValidatorToFile)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- We can create custom data types for our datum and redeemer like this:
newtype MySillyRedeemer = MkMySillyRedeemer Integer
PlutusTx.unstableMakeIsData ''MySillyRedeemer -- Use TH to create an instance for IsData.

-- This validator succeeds only if the redeemer is `MkMySillyRedeemer 42`
--              Datum     Redeemer            ScriptContext
mkCTValidator :: () -> MySillyRedeemer -> PlutusV2.ScriptContext -> Bool
mkCTValidator _ (MkMySillyRedeemer r) _ = traceIfFalse "expected 42" $ r == 42
{-# INLINABLE mkCTValidator #-}

wrappedMkVal :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedMkVal = wrapValidator mkCTValidator
{-# INLINABLE wrappedMkVal #-}

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrappedMkVal ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/customtypes.plutus" validator
