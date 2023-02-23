{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Homework1 where

import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx
import PlutusTx.Prelude 
import Utils (wrap)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{-# INLINABLE mkValidator #-}
-- This should validate if and only if the two Booleans in the redeemer are True!
mkValidator :: () -> (Bool, Bool) -> PlutusV2.ScriptContext -> Bool
mkValidator _ _ _ = False

wrappedVal :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedVal = wrap mkValidator

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrappedVal ||])