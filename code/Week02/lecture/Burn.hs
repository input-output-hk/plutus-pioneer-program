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

-- Type aliases to make the validator's signature more meaningful
type BurnDatum     = BuiltinData
type BurnRedeemer  = BuiltinData
type ScriptContext = BuiltinData

-- This validator always fails
{-# INLINABLE mkBurnValidator #-}
mkBurnValidator :: BurnDatum -> BurnRedeemer -> ScriptContext -> ()
mkBurnValidator _ _ _ = error ()

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mkBurnValidator ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: IO ()
saveVal = writePlutusFile "./assets/burn.plutus" validator