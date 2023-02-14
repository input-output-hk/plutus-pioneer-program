{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils (writePlutusFile) where

import Plutus.V2.Ledger.Api qualified as PlutusV2
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import Cardano.Api.Shelley (PlutusScript (..))
import Cardano.Api
import PlutusTx.Prelude
import Codec.Serialise (serialise)
import Prelude (IO, putStrLn, print, FilePath)

-- Serialize script
serializeScript :: PlutusV2.Validator -> PlutusScript PlutusScriptV2
serializeScript = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise

-- Create file with compiled Plutus script
writePlutusFile :: FilePath -> PlutusV2.Validator -> IO ()
writePlutusFile filePath validator =
  writeFileTextEnvelope filePath Nothing (serializeScript validator) >>= \case
    Left err -> print $ displayError err
    Right _ -> putStrLn $ "Compiled Plutus script at: " ++ filePath