{-# LANGUAGE LambdaCase #-}

module Utilities.Serialise
  ( validatorToScript
  , policyToScript
  , writeValidatorToFile
  , writePolicyToFile
  , dataToJSON
  , printDataToJSON
  , writeDataToFile
  ) where

import           Cardano.Api           (Error (displayError), PlutusScript,
                                        PlutusScriptV2, prettyPrintJSON,
                                        writeFileJSON, writeFileTextEnvelope)
import           Cardano.Api.Shelley   (PlutusScript (..), fromPlutusData,
                                        scriptDataToJsonDetailedSchema)
import           Codec.Serialise       (serialise, Serialise)
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.ByteString.Short as BSS
import           Plutus.V1.Ledger.Api  (ToData)
import qualified Plutus.V2.Ledger.Api  as PlutusV2
import           Text.Printf           (printf)


serializableToScript :: Serialise a => a -> PlutusScript PlutusScriptV2
serializableToScript = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise

-- Serialize validator
validatorToScript :: PlutusV2.Validator -> PlutusScript PlutusScriptV2
validatorToScript = serializableToScript

-- Serialize minting policy
policyToScript :: PlutusV2.MintingPolicy -> PlutusScript PlutusScriptV2
policyToScript = serializableToScript

-- Create file with compiled Plutus validator
writeValidatorToFile :: FilePath -> PlutusV2.Validator -> IO ()
writeValidatorToFile filePath validator =
  writeFileTextEnvelope filePath Nothing (validatorToScript validator) >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "Compiled Plutus script at: " ++ filePath

-- Create file with compiled Plutus minting policy
writePolicyToFile :: FilePath -> PlutusV2.MintingPolicy -> IO ()
writePolicyToFile filePath policy =
  writeFileTextEnvelope filePath Nothing (policyToScript policy) >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "Compiled Plutus script at: " ++ filePath

dataToJSON :: ToData a => a -> Value
dataToJSON = scriptDataToJsonDetailedSchema . fromPlutusData . PlutusV2.toData

printDataToJSON :: ToData a => a -> IO ()
printDataToJSON = putStrLn . BS8.unpack . prettyPrintJSON . dataToJSON

writeDataToFile :: ToData a => FilePath -> a -> IO ()
writeDataToFile filePath x = do
  let v = dataToJSON x
  writeFileJSON filePath v >>= \case
   Left err -> print $ displayError err
   Right () -> printf "Wrote data to: %s\n%s\n" filePath $ BS8.unpack $ prettyPrintJSON v
