{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Week05.Minting where

import           PlutusLedgerApi.Common      (SerialisedScript,
                                              serialiseCompiledCode)
import           PlutusLedgerApi.V1.Value    (flattenValue)
import           PlutusLedgerApi.V3          (ScriptContext (..), TokenName,
                                              TxInInfo (txInInfoOutRef),
                                              TxInfo (txInfoInputs, txInfoMint),
                                              TxOutRef (TxOutRef), TxId (TxId),
                                              PubKeyHash)
import           PlutusTx                    (BuiltinData, CompiledCode,
                                              UnsafeFromData (unsafeFromBuiltinData),
                                              compile)
import           PlutusLedgerApi.V3.Contexts (txSignedBy)
import           PlutusTx.Bool               (Bool (..), (&&))
import           PlutusTx.Prelude            (BuiltinUnit, Eq ((==)), any, check,
                                              traceIfFalse, ($))

{- ------------------------------------------------------------------------------------------ -}
{- --------------------------------------- VALIDATOR ---------------------------------------- -}

{-# INLINABLE signedVal #-}
signedVal :: PubKeyHash -> ScriptContext -> Bool
signedVal pkh ctx = traceIfFalse "missing signature" $ 
                                 txSignedBy (scriptContextTxInfo ctx) pkh

{- ------------------------------------------------------------------------------------------ -}
{- ---------------------------------------- HELPERS ----------------------------------------- -}

compiledSignedVal :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
compiledSignedVal = $$(compile [||wrappedVal||])
 where
  wrappedVal :: BuiltinData -> BuiltinData -> BuiltinUnit
  wrappedVal pkh ctx = check $ signedVal 
                                 (unsafeFromBuiltinData pkh)
                                 (unsafeFromBuiltinData ctx)

serializedSignedVal :: SerialisedScript
serializedSignedVal = serialiseCompiledCode compiledSignedVal

{- ------------------------------------------------------------------------------------------ -}
{- ------------------------------------- NFT VALIDATOR -------------------------------------- -}

{-# INLINEABLE nftVal #-}
nftVal :: TxOutRef -> TokenName -> ScriptContext -> Bool
nftVal oref tn ctx =
  traceIfFalse "UTxO not consumed" checkHasUTxO && 
  traceIfFalse "You can only mint one!" checkMintedAmount
 where
  checkHasUTxO :: Bool
  checkHasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

  checkMintedAmount :: Bool
  checkMintedAmount = case flattenValue (txInfoMint info) of
    [(_, tn', amt)] -> tn' == tn && amt == 1
    _               -> False

  info :: TxInfo
  info = scriptContextTxInfo ctx

{- ------------------------------------------------------------------------------------------ -}
{- ---------------------------------------- HELPERS ----------------------------------------- -}

compiledNftVal :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
compiledNftVal = $$(compile [||wrappedVal||])
 where
  wrappedVal :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
  wrappedVal tid idx tn ctx =
    let oref :: TxOutRef
        oref = TxOutRef
          (TxId $ unsafeFromBuiltinData tid)
          (unsafeFromBuiltinData idx)
    in check $ nftVal
                 oref
                 (unsafeFromBuiltinData tn)
                 (unsafeFromBuiltinData ctx)

serializedNFTVal :: SerialisedScript
serializedNFTVal = serialiseCompiledCode compiledNftVal
