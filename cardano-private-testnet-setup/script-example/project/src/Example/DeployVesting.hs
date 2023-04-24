{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Example.DeployVesting
    ( writeJSON
    , writeValidator
    , writeUnit
    , writeVestingDatumJson
    , writeVestingValidator
    ) where

import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified PlutusTx
import qualified Ledger
import           Plutus.V1.Ledger.Api
import           Data.String

import           Example.Vesting


dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

writeVestingDatumJson :: FilePath -> String -> Integer -> IO ()
writeVestingDatumJson outFilePath beneficiaryPkh deadlineMillis = writeJSON outFilePath datum
    where datum = VestingDatum {
                    beneficiary = Ledger.PaymentPubKeyHash (fromString beneficiaryPkh)
                  , deadline    = Ledger.POSIXTime deadlineMillis
                  }

writeUnit :: FilePath -> IO ()
writeUnit outFilePath = writeJSON outFilePath ()

writeVestingValidator :: FilePath -> IO (Either (FileError ()) ())
writeVestingValidator outFilePath = writeValidator outFilePath $ validator