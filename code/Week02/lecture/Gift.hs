{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Gift where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx             (BuiltinData, compile)
import           Prelude              (IO)
import           Utilities            (writeValidatorToFile)

---------------------------------------------------------------------------------------------------
-------------------------------- ON-CHAIN CODE / VALIDATOR ----------------------------------------

-- This validator always succeeds
--                    Datum         Redeemer     ScriptContext
mkGiftValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkGiftValidator _ _ _ = ()
{-# INLINABLE mkGiftValidator #-}

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mkGiftValidator ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/gift.plutus" validator