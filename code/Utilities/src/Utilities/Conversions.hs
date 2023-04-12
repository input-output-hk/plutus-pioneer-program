module Utilities.Conversions
  ( Network (..)
  , validatorHash
  , policyHash
  , currencySymbol
  , validatorAddressBech32
  , posixTimeFromIso8601
  , posixTimeToIso8601
  , bytesFromHex
  , bytesToHex
  ) where

import qualified Cardano.Api                as Api
import qualified Cardano.Api.Shelley        as Api
import           Cardano.Ledger.BaseTypes   (Network (..))
import           Cardano.Ledger.Credential  (Credential (ScriptHashObj),
                                             StakeReference (StakeRefNull))
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base16     as BS16
import qualified Data.Text                  as Text
import qualified Data.Time.Clock.POSIX      as Time
import qualified Data.Time.Format.ISO8601   as Time
import           Plutus.V2.Ledger.Api       (CurrencySymbol (CurrencySymbol),
                                             MintingPolicy,
                                             MintingPolicyHash (MintingPolicyHash),
                                             POSIXTime, Validator)
import           PlutusTx.Builtins.Internal (BuiltinByteString (..))
import           Utilities.Serialise        (policyToScript, validatorToScript)

hashScript :: Api.PlutusScript Api.PlutusScriptV2 -> Api.ScriptHash
hashScript = Api.hashScript . Api.PlutusScript Api.PlutusScriptV2

validatorHash :: Validator -> Api.ScriptHash
validatorHash = hashScript . validatorToScript

policyHash :: MintingPolicy -> MintingPolicyHash
policyHash = MintingPolicyHash . BuiltinByteString . Api.serialiseToRawBytes . hashScript . policyToScript

currencySymbol :: MintingPolicy -> CurrencySymbol
currencySymbol = CurrencySymbol . BuiltinByteString . Api.serialiseToRawBytes . hashScript . policyToScript

validatorAddressBech32 :: Network -> Validator -> String
validatorAddressBech32 network v =
    Text.unpack $
    Api.serialiseToBech32 $
    Api.ShelleyAddress
      network
      (ScriptHashObj $ Api.toShelleyScriptHash $ validatorHash v)
      StakeRefNull

posixTimeFromIso8601 :: String -> Maybe POSIXTime
posixTimeFromIso8601 s = do
    t <- Time.formatParseM Time.iso8601Format s
    let seconds = Time.utcTimeToPOSIXSeconds t
        milliSeconds = round $ 1000 * seconds :: Integer
    return $ fromInteger milliSeconds

posixTimeToIso8601 :: POSIXTime -> String
posixTimeToIso8601 t = Time.formatShow Time.iso8601Format $ Time.posixSecondsToUTCTime $ fromRational $ toRational t / 1000

bytesFromHex :: BS.ByteString -> BS.ByteString
bytesFromHex = either error id . BS16.decode

bytesToHex :: BS.ByteString -> BS.ByteString
bytesToHex = BS16.encode
