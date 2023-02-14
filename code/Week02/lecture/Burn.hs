{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Burn where

import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx
import PlutusTx.Prelude (error)
import Utils (writePlutusFile)
import Prelude (IO)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- This validator always fails
--                    Datum         Redeemer     ScriptContext
mkBurnValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkBurnValidator _ _ _ = error ()
{-# INLINABLE mkBurnValidator #-}

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mkBurnValidator ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: IO ()
saveVal = writePlutusFile "./assets/burn.plutus" validator