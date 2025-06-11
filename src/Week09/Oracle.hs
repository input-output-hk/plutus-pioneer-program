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

module Week09.Oracle where

import           GHC.Generics                (Generic)
import           PlutusLedgerApi.Common      (SerialisedScript,
                                              serialiseCompiledCode)
import           PlutusLedgerApi.V1.Value    (AssetClass, assetClassValueOf)
import           PlutusLedgerApi.V3          (Datum (..), OutputDatum (..),
                                              PubKeyHash, Redeemer (..),
                                              ScriptContext (..), TxInInfo (..),
                                              TxInfo (..), TxOut (..))
import           PlutusLedgerApi.V3.Contexts (findDatum, findOwnInput,
                                              getContinuingOutputs, txSignedBy)
import           PlutusTx                    (BuiltinData, CompiledCode,
                                              FromData (..),
                                              UnsafeFromData (unsafeFromBuiltinData),
                                              compile, makeIsDataSchemaIndexed,
                                              makeLift)
import           PlutusTx.Blueprint          (HasBlueprintDefinition,
                                              definitionRef)
import           PlutusTx.Bool               (Bool (..), (&&))
import           PlutusTx.Prelude            (BuiltinUnit, Eq (..), Integer,
                                              Maybe (..), check, isJust,
                                              traceError, traceIfFalse, ($))

{- ------------------------------------------------------------------------------------------ -}
{- ----------------------------------------- TYPES ------------------------------------------ -}

data OracleParams = OracleParams
  { oNFT      :: AssetClass
  , oOperator :: PubKeyHash
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

makeLift ''OracleParams
makeIsDataSchemaIndexed ''OracleParams [('OracleParams, 0)]

data OracleRedeemer = Update | Delete
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''OracleRedeemer [('Update, 0), ('Delete, 1)]

-- Oracle Datum
type Rate = Integer

{- ------------------------------------------------------------------------------------------ -}
{- --------------------------- ON-CHAIN: HELPER FUNCTIONS/TYPES ----------------------------- -}

{-# INLINEABLE parseOracleDatum #-}
parseOracleDatum :: TxOut -> TxInfo -> Maybe Integer
parseOracleDatum o info = case txOutDatum o of
  NoOutputDatum -> Nothing
  OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d
  OutputDatumHash dh -> do
    Datum d <- findDatum dh info
    PlutusTx.fromBuiltinData d

{- ------------------------------------------------------------------------------------------ -}
{- --------------------------------------- VALIDATOR ---------------------------------------- -}

{- NOTE: I'm not taking advatage of CIP-69 to embed NFT logic in here. I don't think it would
 - help much. It might even be detrimental because I'd use a oneshot logic that would then
 - stick around forever without any use.
 - -}

{-# INLINEABLE oracleVal #-}
oracleVal :: OracleParams -> ScriptContext -> Bool
oracleVal params ctx =
  case r of
    Update ->
      traceIfFalse "token missing from input" inputHasToken
        && traceIfFalse "token missing from output" outputHasToken
        && traceIfFalse "operator signature missing" checkOperatorSignature
        && traceIfFalse "invalid output datum" validOutputDatum
    Delete -> traceIfFalse "operator signature missing" checkOperatorSignature
 where
  info :: TxInfo
  info = scriptContextTxInfo ctx

  r = case fromBuiltinData $ getRedeemer (scriptContextRedeemer ctx) of
    Just @OracleRedeemer n -> n
    Nothing                -> traceError "Redeemer is not a number"

  -- Check that the 'oracle' is signed by the 'oOperator'.
  checkOperatorSignature :: Bool
  checkOperatorSignature = txSignedBy info $ oOperator params

  -- Find the oracle input.
  ownInput :: TxOut
  ~ownInput = case findOwnInput ctx of
    Nothing -> traceError "oracle input missing"
    Just i  -> txInInfoResolved i

  -- Check that the oracle input contains the NFT.
  inputHasToken :: Bool
  ~inputHasToken = assetClassValueOf (txOutValue ownInput) (oNFT params) == 1

  -- Find the oracle output.
  ownOutput :: TxOut
  ~ownOutput = case getContinuingOutputs ctx of
    [o] -> o
    _   -> traceError "expected exactly one oracle output"

  -- Check that the oracle output contains the NFT.
  outputHasToken :: Bool
  ~outputHasToken = assetClassValueOf (txOutValue ownOutput) (oNFT params) == 1

  -- Check that the oracle output contains a valid datum.
  validOutputDatum :: Bool
  ~validOutputDatum = isJust $ parseOracleDatum ownOutput info

{- ------------------------------------------------------------------------------------------ -}
{- ---------------------------------------- HELPERS ----------------------------------------- -}

compiledVal :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
compiledVal = $$(compile [||wrappedVal||])
 where
  wrappedVal :: BuiltinData -> BuiltinData -> BuiltinUnit
  wrappedVal params ctx = PlutusTx.Prelude.check $ oracleVal (unsafeFromBuiltinData params) (unsafeFromBuiltinData ctx)

serializedOracleVal :: SerialisedScript
serializedOracleVal = serialiseCompiledCode compiledVal
