{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeApplications  #-}

module Week10.Deploy
    ( writeStakeValidator
    , tryReadAddress
    ) where

import           Cardano.Api                 as API
import           Cardano.Api.Shelley         (Address (..), PlutusScript (..))
import           Cardano.Crypto.Hash.Class   (hashToBytes)
import           Cardano.Ledger.Credential   as Ledger
import           Cardano.Ledger.Crypto       (StandardCrypto)
import           Cardano.Ledger.Hashes       (ScriptHash (..))
import           Cardano.Ledger.Keys         (KeyHash (..))
import           Codec.Serialise             (serialise)
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Short       as SBS
import           Data.Text                   (pack)
import           Plutus.V1.Ledger.Credential as Plutus
import           Plutus.V1.Ledger.Crypto     as Plutus
import           PlutusTx.Builtins           (toBuiltin)
import qualified Ledger                      as Plutus

import           Week10.Staking

writeStakeValidator :: FilePath -> Plutus.Address -> IO (Either (FileError ()) ())
writeStakeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.getStakeValidator . stakeValidator

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
