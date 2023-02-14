{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}

module FortyTwo where

import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx
import PlutusTx.Prelude (otherwise, (==))
import Utils (writePlutusFile)
import Prelude (IO)
import PlutusTx.Builtins as Builtins

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- Type aliases to make the validator's signature more meaningful
type GiftDatum     = BuiltinData
type GiftRedeemer  = BuiltinData
type ScriptContext = BuiltinData

-- This validator succeeds only if the redeemer is 42
{-# INLINABLE mk42Validator #-}
mk42Validator :: GiftDatum -> GiftRedeemer -> ScriptContext -> ()
mk42Validator _ r _
    | r == Builtins.mkI 42 = ()
    | otherwise            = error ()

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mk42Validator ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: IO ()
saveVal = writePlutusFile "./assets/fortytwo.plutus" validator