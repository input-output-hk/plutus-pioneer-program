{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module NFT where

import qualified Data.ByteString.Char8      as BS8
import           Plutus.V1.Ledger.Value     (flattenValue)
import           Plutus.V2.Ledger.Api       (BuiltinData, CurrencySymbol,
                                             MintingPolicy,
                                             ScriptContext (scriptContextTxInfo),
                                             TokenName (unTokenName),
                                             TxId (TxId, getTxId),
                                             TxInInfo (txInInfoOutRef),
                                             TxInfo (txInfoInputs, txInfoMint),
                                             TxOutRef (TxOutRef, txOutRefId, txOutRefIdx),
                                             mkMintingPolicyScript)
import qualified PlutusTx
import           PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import           PlutusTx.Prelude           (Bool (False), Eq ((==)), any,
                                             traceIfFalse, ($), (&&))
import           Prelude                    (IO, Show (show), String)
import           Text.Printf                (printf)
import           Utilities                  (bytesToHex, currencySymbol,
                                             wrapPolicy, writeCodeToFile,
                                             writePolicyToFile)

{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkNFTPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                             traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn'', amt)] -> tn'' == tn && amt == 1
        _                -> False

{-# INLINABLE mkWrappedNFTPolicy #-}
mkWrappedNFTPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedNFTPolicy tid ix tn' = wrapPolicy $ mkNFTPolicy oref tn
  where
    oref :: TxOutRef
    oref = TxOutRef
        (TxId $ PlutusTx.unsafeFromBuiltinData tid)
        (PlutusTx.unsafeFromBuiltinData ix)

    tn :: TokenName
    tn = PlutusTx.unsafeFromBuiltinData tn'

nftCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
nftCode = $$(PlutusTx.compile [|| mkWrappedNFTPolicy ||])

nftPolicy :: TxOutRef -> TokenName -> MintingPolicy
nftPolicy oref tn = mkMintingPolicyScript $
    nftCode
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ getTxId $ txOutRefId oref)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ txOutRefIdx oref)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData tn)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveNFTCode :: IO ()
saveNFTCode = writeCodeToFile "assets/nft.plutus" nftCode

saveNFTPolicy :: TxOutRef -> TokenName -> IO ()
saveNFTPolicy oref tn = writePolicyToFile
    (printf "assets/nft-%s#%d-%s.plutus"
        (show $ txOutRefId oref)
        (txOutRefIdx oref) $
        tn') $
    nftPolicy oref tn
  where
    tn' :: String
    tn' = case unTokenName tn of
        (BuiltinByteString bs) -> BS8.unpack $ bytesToHex bs

nftCurrencySymbol :: TxOutRef -> TokenName -> CurrencySymbol
nftCurrencySymbol oref tn = currencySymbol $ nftPolicy oref tn
