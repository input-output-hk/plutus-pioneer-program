{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}

module Week06.Utils
    ( tryReadAddress, unsafeReadAddress
    , tryReadWalletId, unsafeReadWalletId
    , unsafeReadTxOutRef
    , writeJSON, writeUnit
    , contractActivationArgs
    , getCredentials, unsafePaymentPubKeyHash, unsafeStakePubKeyHash
    , cidToString
    , writeMintingPolicy
    , unsafeTokenNameToHex
    ) where

import           Cardano.Api                 as API
import           Cardano.Api.Shelley         (Address (..), PlutusScript (..))
import           Cardano.Crypto.Hash.Class   (hashToBytes)
import           Cardano.Ledger.Credential   as Ledger
import           Cardano.Ledger.Crypto       (StandardCrypto)
import           Cardano.Ledger.Hashes       (ScriptHash (..))
import           Cardano.Ledger.Keys         (KeyHash (..))
import           Codec.Serialise             (serialise)
import           Data.Aeson                  (decode, encode)
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Short       as SBS
import           Data.Maybe                  (fromJust, fromMaybe)
import           Data.String                 (IsString (..))
import           Data.Text                   (pack)
import           Plutus.PAB.Webserver.Types  (ContractActivationArgs (..))
import           Plutus.V1.Ledger.Credential as Plutus
import           Plutus.V1.Ledger.Crypto     as Plutus
import           Plutus.V1.Ledger.Value      (TokenName (..))
import           PlutusTx                    (Data (..))
import qualified PlutusTx
import           PlutusTx.Builtins           (toBuiltin)
import           PlutusTx.Builtins.Internal  (BuiltinByteString (..))
import qualified Ledger                      as Plutus
import           Wallet.Emulator.Wallet      (WalletId (..), Wallet (..))
import           Wallet.Types                (ContractInstanceId (..))

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

tryReadWalletId :: String -> Maybe WalletId
tryReadWalletId = decode . encode


unsafeReadWalletId :: String -> WalletId
unsafeReadWalletId s = fromMaybe (error $ "can't parse " ++ s ++ " as a WalletId") $ tryReadWalletId s

unsafeReadAddress :: String -> Plutus.Address
unsafeReadAddress s = fromMaybe (error $ "can't parse " ++ s ++ " as an address") $ tryReadAddress s

unsafeReadTxOutRef :: String -> Plutus.TxOutRef
unsafeReadTxOutRef s =
  let
    (x, _ : y) = span (/= '#') s
  in
    Plutus.TxOutRef
        { Plutus.txOutRefId  = fromString x
        , Plutus.txOutRefIdx = read y
        }

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeUnit :: IO ()
writeUnit = writeJSON "testnet/unit.json" ()

contractActivationArgs :: WalletId -> a -> ContractActivationArgs a
contractActivationArgs wid a = ContractActivationArgs
    { caID = a
    , caWallet = Just $ Wallet {getWalletId = wid}
    }

getCredentials :: Plutus.Address -> Maybe (Plutus.PaymentPubKeyHash, Maybe Plutus.StakePubKeyHash)
getCredentials (Plutus.Address x y) = case x of
    ScriptCredential _   -> Nothing
    PubKeyCredential pkh ->
      let
        ppkh = Plutus.PaymentPubKeyHash pkh
      in
        case y of
            Nothing                        -> Just (ppkh, Nothing)
            Just (Plutus.StakingPtr _ _ _) -> Nothing
            Just (StakingHash h)           -> case h of
                ScriptCredential _    -> Nothing
                PubKeyCredential pkh' -> Just (ppkh, Just $ Plutus.StakePubKeyHash pkh')

unsafePaymentPubKeyHash :: Plutus.Address -> Plutus.PaymentPubKeyHash
unsafePaymentPubKeyHash addr = maybe (error $ "script address " ++ show addr ++ " does not contain a payment key") fst $ getCredentials addr

unsafeStakePubKeyHash :: Plutus.Address -> Plutus.StakePubKeyHash
unsafeStakePubKeyHash addr = case getCredentials addr of
    Nothing           -> error $ "unexpected script address " ++ show addr
    Just (_, Nothing) -> error $ "addres " ++ show addr ++ " contains no stake component"
    Just (_, Just x)  -> x

cidToString :: ContractInstanceId -> String
cidToString = show . unContractInstanceId

writeMintingPolicy :: FilePath -> Plutus.MintingPolicy -> IO (Either (FileError ()) ())
writeMintingPolicy file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.getMintingPolicy

unsafeTokenNameToHex :: TokenName -> String
unsafeTokenNameToHex = BS8.unpack . serialiseToRawBytesHex . fromJust . deserialiseFromRawBytes AsAssetName . getByteString . unTokenName
  where
    getByteString (BuiltinByteString bs) = bs
