{-# LANGUAGE LambdaCase #-}

module Utilities.Serialise
  ( validatorToScript
  , writeValidatorToFile
  , writeDataToFile
  ) where

import           Cardano.Api           (Error (displayError), PlutusScript,
                                        PlutusScriptV2, prettyPrintJSON,
                                        writeFileJSON, writeFileTextEnvelope)
import           Cardano.Api.Shelley   (PlutusScript (..), fromPlutusData,
                                        scriptDataToJsonDetailedSchema)
import           Codec.Serialise       (serialise)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.ByteString.Short as BSS
import           Plutus.V1.Ledger.Api  (ToData)
import qualified Plutus.V2.Ledger.Api  as PlutusV2
import           Text.Printf           (printf)

-- Serialize script
validatorToScript :: PlutusV2.Validator -> PlutusScript PlutusScriptV2
validatorToScript = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise

-- Create file with compiled Plutus script
writeValidatorToFile :: FilePath -> PlutusV2.Validator -> IO ()
writeValidatorToFile filePath validator =
  writeFileTextEnvelope filePath Nothing (validatorToScript validator) >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "Compiled Plutus script at: " ++ filePath

writeDataToFile :: ToData a => FilePath -> a -> IO ()
writeDataToFile filePath x = do
  let v = scriptDataToJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData x
  writeFileJSON filePath v >>= \case
   Left err -> print $ displayError err
   Right () -> printf "Wrote data to: %s\n%s\n" filePath $ BS8.unpack $ prettyPrintJSON v
