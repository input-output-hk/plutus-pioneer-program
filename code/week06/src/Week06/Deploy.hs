{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Week06.Deploy
    ( writeJSON
    , writeOracleParams
    , walletAddress
    , oracleParams
    , writeTokenParams
    , tokenParams
    ) where

import           Cardano.Api                 as API
import           Cardano.Api.Shelley         (Address (..))
import           Cardano.Crypto.Hash.Class   (hashToBytes)
import           Cardano.Ledger.Credential   as Ledger
import           Cardano.Ledger.Crypto       (StandardCrypto)
import           Cardano.Ledger.Hashes       (ScriptHash (..))
import           Cardano.Ledger.Keys         (KeyHash (..))
import           Data.Aeson                  (encode)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (pack)
import           Plutus.V1.Ledger.Credential as Plutus
import           Plutus.V1.Ledger.Crypto     as Plutus
import           PlutusTx                    (Data (..))
import qualified PlutusTx
import           PlutusTx.Builtins           (toBuiltin)
import qualified Ledger                      as Plutus

import           Week06.Core                 (OracleParams (..))
import           Week06.Token                (TokenParams (..))

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

credentialLedgerToPlutus :: Ledger.Credential a StandardCrypto -> Plutus.Credential
credentialLedgerToPlutus (ScriptHashObj (ScriptHash h)) = Plutus.ScriptCredential $ Plutus.ValidatorHash $ toBuiltin $ hashToBytes h
credentialLedgerToPlutus (KeyHashObj (KeyHash h))       = Plutus.PubKeyCredential $ Plutus.PubKeyHash $ toBuiltin $ hashToBytes h

stakeReferenceLedgerToPlutus :: Ledger.StakeReference StandardCrypto -> Maybe Plutus.StakingCredential
stakeReferenceLedgerToPlutus (StakeRefBase x)                   = Just $ StakingHash $ credentialLedgerToPlutus x
stakeReferenceLedgerToPlutus (StakeRefPtr (Ptr (SlotNo x) y z)) = Just $ StakingPtr (fromIntegral x) (fromIntegral y) (fromIntegral z)
stakeReferenceLedgerToPlutus StakeRefNull                       = Nothing

tryReadAddress :: String -> Maybe Plutus.Address
tryReadAddress x = case deserialiseAddress AsAddressAny $ pack x of
    Nothing                                      -> Nothing
    Just (AddressByron _)                        -> Nothing
    Just (AddressShelley (ShelleyAddress _ p s)) -> Just Plutus.Address
        { Plutus.addressCredential        = credentialLedgerToPlutus p
        , Plutus.addressStakingCredential = stakeReferenceLedgerToPlutus s
        }

walletAddress :: Plutus.Address
walletAddress = unsafeReadAddress "addr_test1qzj356wpdmhdchvmc355xx6wel7cqvepyrlam84aygkvx9d04w7v8cu4fshxvv5ukfw05nyzh07zy427mf2eqkcd27aqax2r7e"

unsafeReadAddress :: String -> Plutus.Address
unsafeReadAddress s = fromMaybe (error $ "can't parse " ++ s ++ " as an address") $ tryReadAddress s

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeOracleParams :: FilePath -> OracleParams -> IO ()
writeOracleParams file = LBS.writeFile file . encode

oracleParams :: OracleParams
oracleParams = OracleParams
    { opFees    = 1_000_000
    , opSymbol  = "71066256d2f4850c731819a8e6de155c97ea1ee53dd3dd903f8e3258"
    , opToken   = "USDT"
    , opAddress = walletAddress
    }

writeTokenParams :: FilePath -> TokenParams -> IO ()
writeTokenParams file = LBS.writeFile file . encode

tokenParams :: TokenParams
tokenParams = TokenParams
    { tpToken   = "USDT"
    , tpAmount  = 100_000
    , tpAddress = walletAddress
    }
