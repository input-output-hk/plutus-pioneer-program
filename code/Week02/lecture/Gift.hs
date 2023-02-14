{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Gift where

import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx
import PlutusTx.Prelude
import Cardano.Api
import Prelude (IO, putStrLn, print)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- Type aliases to make the validator's signature more readable
type GiftDatum = BuiltinData
type GiftRedeemer = BuiltinData
type ScriptContext = BuiltinData

-- This validator always succeeds
{-# INLINABLE mkGiftValidator #-}
mkGiftValidator :: GiftDatum -> GiftRedeemer -> ScriptContext -> ()
mkGiftValidator _ _ _ = ()

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mkGiftValidator ||])

---------------------------------------------------------------------------------------------------
-------------------------------------- HELPER FUNCTIONS -------------------------------------------

-- Serialize script
serializedScript :: PlutusScript PlutusScriptV2
serializedScript = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise $ validator

-- Create file with compiled Plutus script
writePlutusFile :: IO ()
writePlutusFile =
  writeFileTextEnvelope filePath Nothing serializedScript >>= \case
    Left err -> print $ displayError err
    Right _ -> putStrLn $ "Compiled Plutus script at: " ++ filePath
  where
    filePath = "gift.plutus"