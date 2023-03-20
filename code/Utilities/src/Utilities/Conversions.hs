module Utilities.Conversions
  ( Network (..)
  , validatorHash
  , validatorAddressBech32
  , posixTimeFromIso8601
  , posixTimeToIso8601
  , bytesFromHex
  ) where

import qualified Cardano.Api               as Api
import qualified Cardano.Api.Shelley       as Api
import           Cardano.Ledger.BaseTypes  (Network (..))
import           Cardano.Ledger.Credential (Credential (ScriptHashObj),
                                            StakeReference (StakeRefNull))
import qualified Data.Text                 as Text
import qualified Data.Time.Clock.POSIX     as Time
import qualified Data.Time.Format.ISO8601  as Time
import           Plutus.V2.Ledger.Api      (POSIXTime, Validator)
import           Utilities.Serialise       (validatorToScript)
import qualified Data.ByteString           as BS
import qualified Plutus.V1.Ledger.Bytes    as P

validatorHash :: Validator -> Api.ScriptHash
validatorHash v = Api.hashScript $ Api.PlutusScript Api.PlutusScriptV2 $ validatorToScript v

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
bytesFromHex = P.bytes . fromEither . P.fromHex
  where
    fromEither (Left e)  = Prelude.error $ show e
    fromEither (Right b) = b