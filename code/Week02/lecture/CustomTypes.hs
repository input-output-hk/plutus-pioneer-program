{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}

module CustomTypes where

import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx
import PlutusTx.Prelude 
import Utils (writePlutusFile, wrap)
import Prelude (IO)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- We can create custom data types for our datum and redeemer like this:
newtype MySillyRedeemer = MkMySillyRedeemer Integer
PlutusTx.unstableMakeIsData ''MySillyRedeemer -- Use TH to create an instance for IsData.

-- This validator succeeds only if the redeemer is `MkMySillyRedeemer 42`
--              Datum     Redeemer            ScriptContext
mkCTValidator :: () -> MySillyRedeemer -> PlutusV2.ScriptContext -> Bool
mkCTValidator _ (MkMySillyRedeemer r) _ = r == 42
{-# INLINABLE mkCTValidator #-}

wrappedMkVal :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedMkVal = wrap mkCTValidator

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrappedMkVal ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: IO ()
saveVal = writePlutusFile "./assets/customtypes.plutus" validator