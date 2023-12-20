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
import           PlutusTx.Prelude       (Bool (..), ($), (.), (&&), 
                                         (==), any, traceIfFalse)
import           Utilities              (wrapPolicy, writeCodeToFile)
import           Prelude (IO)

{-# INLINABLE tn #-}
tn :: TokenName
tn = TokenName emptyByteString

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
        [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == tn && amt == 1
        _                -> False

{-# INLINABLE mkWrappedEmptyNFTPolicy #-}
mkWrappedEmptyNFTPolicy :: TxOutRef -> BuiltinData -> BuiltinData -> ()
mkWrappedEmptyNFTPolicy = wrapPolicy . mkEmptyNFTPolicy

nftCode :: PlutusTx.CompiledCode (TxOutRef -> BuiltinData -> BuiltinData -> ())
nftCode = $$(PlutusTx.compile [|| mkWrappedEmptyNFTPolicy ||])

nftPolicy :: TxOutRef -> MintingPolicy
nftPolicy oref = mkMintingPolicyScript $ 
    $$(PlutusTx.compile [|| mkWrappedEmptyNFTPolicy ||]) 
        `PlutusTx.applyCode` PlutusTx.liftCode oref

---------------------------------------------------------------------------------
--------------------------------- HELPER FUNCTIONS ------------------------------
saveNFTCode :: IO ()
saveNFTCode = writeCodeToFile "assets/nftPolicyH2_typed.plutus" nftCode