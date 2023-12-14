{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Homework2 where

import           Plutus.V2.Ledger.Api 
import           Plutus.V1.Ledger.Value (flattenValue)
import           Plutus.V2.Ledger.Contexts (ownCurrencySymbol)
import qualified PlutusTx
import           PlutusTx.Builtins.Internal (emptyByteString)
import           PlutusTx.Prelude       (Bool (..), ($), (&&), 
                                         (==), any, traceIfFalse)
import           Utilities              (wrapPolicy, writeCodeToFile)
import           Prelude (IO)

{-# INLINABLE tnEmptyBS #-}
tnEmptyBS :: TokenName
tnEmptyBS = TokenName emptyByteString

{-# INLINABLE mkEmptyNFTPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
mkEmptyNFTPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkEmptyNFTPolicy oref () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                               traceIfFalse "wrong amount minted" checkMintedAmount 
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == tnEmptyBS && amt == 1
        _                -> False

{-# INLINABLE mkWrappedEmptyNFTPolicy #-}
mkWrappedEmptyNFTPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedEmptyNFTPolicy tid ix = wrapPolicy $ mkEmptyNFTPolicy oref 
  where
    oref :: TxOutRef
    oref = TxOutRef
        (TxId $ PlutusTx.unsafeFromBuiltinData tid)
        (PlutusTx.unsafeFromBuiltinData ix)

nftCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
nftCode = $$(PlutusTx.compile [|| mkWrappedEmptyNFTPolicy ||])

nftPolicy :: TxOutRef -> MintingPolicy
nftPolicy oref = mkMintingPolicyScript $
    nftCode
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ getTxId $ txOutRefId oref)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ txOutRefIdx oref)

---------------------------------------------------------------------------------
--------------------------------- HELPER FUNCTIONS ------------------------------
saveNFTCode :: IO ()
saveNFTCode = writeCodeToFile "assets/nftPolicyH2.plutus" nftCode
