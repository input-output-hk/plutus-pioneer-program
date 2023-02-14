{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Gift where

import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx
import Utils (writePlutusFile)
import Prelude (IO)

---------------------------------------------------------------------------------------------------
-------------------------------- ON-CHAIN CODE / VALIDATOR ----------------------------------------

-- Type aliases to make the validator's signature more meaningful
type GiftDatum     = BuiltinData
type GiftRedeemer  = BuiltinData
type ScriptContext = BuiltinData

-- This validator always succeeds
{-# INLINABLE mkGiftValidator #-}
mkGiftValidator :: GiftDatum -> GiftRedeemer -> ScriptContext -> ()
mkGiftValidator _ _ _ = ()

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mkGiftValidator ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: IO ()
saveVal = writePlutusFile "./assets/gift.plutus" validator