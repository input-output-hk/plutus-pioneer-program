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
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Week03.Vesting where

import           GHC.Generics                  (Generic)
import           PlutusLedgerApi.Common        (FromData (fromBuiltinData),
                                                SerialisedScript,
                                                serialiseCompiledCode)
import           PlutusLedgerApi.Data.V3       (POSIXTime, PubKeyHash)
import           PlutusLedgerApi.V1.Interval   (contains, from)
import           PlutusLedgerApi.V3            (ScriptContext (..),
                                                ScriptInfo (..),
                                                TxInfo (txInfoValidRange),
                                                getDatum)
import           PlutusLedgerApi.V3.Contexts   (txSignedBy)
import           PlutusTx                      (BuiltinData, CompiledCode,
                                                UnsafeFromData (unsafeFromBuiltinData),
                                                compile,
                                                makeIsDataSchemaIndexed,
                                                makeLift)
import           PlutusTx.Blueprint            (HasBlueprintDefinition)
import           PlutusTx.Blueprint.Definition (definitionRef)
import           PlutusTx.Bool                 (Bool (..), (&&))
import           PlutusTx.Prelude              (BuiltinUnit, Maybe (..), check,
                                                traceError, traceIfFalse, ($),
                                                (.))

{- ------------------------------------------------------------------------------------------ -}
{- ----------------------------------------- TYPES ------------------------------------------ -}

data VestingDatum = VestingDatum
  { beneficiary :: PubKeyHash
  , deadline    :: POSIXTime
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''VestingDatum [('VestingDatum, 0)]

{- ------------------------------------------------------------------------------------------ -}
{- --------------------------------------- VALIDATOR ---------------------------------------- -}

{-# INLINEABLE vestingVal #-}
vestingVal :: ScriptContext -> Bool
vestingVal ctx =
  traceIfFalse "Is not the beneficiary" checkBeneficiary
    && traceIfFalse "Deadline not reached" checkDeadline
 where
  checkBeneficiary :: Bool
  checkBeneficiary = txSignedBy info (beneficiary vestingDatum)

  checkDeadline :: Bool
  checkDeadline = from (deadline vestingDatum) `contains` txInfoValidRange info

  vestingDatum :: VestingDatum
  vestingDatum = case scriptContextScriptInfo ctx of
    SpendingScript _txRef (Just datum) -> case (fromBuiltinData @VestingDatum . getDatum) datum of
      Just d  -> d
      Nothing -> traceError "Expected correctly shaped datum"
    _ -> traceError "Expected SpendingScript with datum"

  info :: TxInfo
  info = scriptContextTxInfo ctx

{- ------------------------------------------------------------------------------------------ -}
{- ---------------------------------------- HELPERS ----------------------------------------- -}

compiledVestingVal :: CompiledCode (BuiltinData -> BuiltinUnit)
compiledVestingVal = $$(compile [||wrappedVal||])
 where
  wrappedVal :: BuiltinData -> BuiltinUnit
  wrappedVal ctx = check $ vestingVal (unsafeFromBuiltinData ctx)

serializedVestingVal :: SerialisedScript
serializedVestingVal = serialiseCompiledCode compiledVestingVal

{- ------------------------------------------------------------------------------------------ -}
{- ---------------------------------- PARAMETERIZED TYPES ----------------------------------- -}

data VestingParam = VestingParam
  { beneficiaryParam :: PubKeyHash
  , deadlineParam    :: POSIXTime
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

makeLift ''VestingParam
makeIsDataSchemaIndexed ''VestingParam [('VestingParam, 0)]

{- ------------------------------------------------------------------------------------------ -}
{- -------------------------------- PARAMETERIZED VALIDATOR --------------------------------- -}

{-# INLINEABLE paramVestingVal #-}
paramVestingVal :: VestingParam -> ScriptContext -> Bool
paramVestingVal vp ctx =
  traceIfFalse "Is not the beneficiary" checkBeneficiary
    && traceIfFalse "Deadline not reached" checkDeadline
 where
  checkBeneficiary :: Bool
  checkBeneficiary = txSignedBy info (beneficiaryParam vp)

  checkDeadline :: Bool
  checkDeadline = from (deadlineParam vp) `contains` txInfoValidRange info

  info :: TxInfo
  info = scriptContextTxInfo ctx

{- ------------------------------------------------------------------------------------------ -}
{- ---------------------------------------- HELPERS ----------------------------------------- -}

compiledParamVestingVal :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
compiledParamVestingVal = $$(compile [||wrappedVal||])
 where
  wrappedVal :: BuiltinData -> BuiltinData -> PlutusTx.Prelude.BuiltinUnit
  wrappedVal param ctx = check $ paramVestingVal (unsafeFromBuiltinData param) (unsafeFromBuiltinData ctx)

serializedParamVestingVal :: SerialisedScript
serializedParamVestingVal = serialiseCompiledCode compiledParamVestingVal

{- ------------------------------------------------------------------------------------------ -}
{- ---------------------------- 2-TIMES PARAMETERIZED VALIDATOR ----------------------------- -}

{-# INLINEABLE param2VestingVal #-}
param2VestingVal :: PubKeyHash -> POSIXTime -> ScriptContext -> Bool
param2VestingVal keyHash time ctx =
  traceIfFalse "Is not the beneficiary" checkBeneficiary
    && traceIfFalse "Deadline not reached" checkDeadline
 where
  checkBeneficiary :: Bool
  checkBeneficiary = txSignedBy info keyHash

  checkDeadline :: Bool
  checkDeadline = from time `contains` txInfoValidRange info

  info :: TxInfo
  info = scriptContextTxInfo ctx

{- ------------------------------------------------------------------------------------------ -}
{- ---------------------------------------- HELPERS ----------------------------------------- -}

compiledParam2VestingVal :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
compiledParam2VestingVal = $$(compile [||wrappedVal||])
 where
  wrappedVal :: BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.Prelude.BuiltinUnit
  wrappedVal param1 param2 ctx = check $ param2VestingVal (unsafeFromBuiltinData param1) 
                                                          (unsafeFromBuiltinData param2)
                                                          (unsafeFromBuiltinData ctx)

serializedParam2VestingVal :: SerialisedScript
serializedParam2VestingVal = serialiseCompiledCode compiledParam2VestingVal

{- ------------------------------------------------------------------------------------------ -}
{- ----------------------------------------- TYPES ------------------------------------------ -}

data VestingDatumMix = VestingDatum1 { beneficiary1 :: PubKeyHash,
                                       deadline1 :: POSIXTime }
                     | VestingDatum2 { deadline2 :: POSIXTime,
                                       beneficiary2 :: PubKeyHash }
                     deriving stock (Generic)
                     deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''VestingDatumMix [('VestingDatum1, 0), ('VestingDatum2, 1)]

{- ------------------------------------------------------------------------------------------ -}
{- --------------------------------- VALIDATOR MIXED DATUM ---------------------------------- -}

{-# INLINEABLE vestingValMix #-}
vestingValMix :: ScriptContext -> Bool
vestingValMix ctx =
  traceIfFalse "Is not the beneficiary" checkBeneficiary
    && traceIfFalse "Deadline not reached" checkDeadline
 where
  checkBeneficiary :: Bool
  checkBeneficiary = txSignedBy info beneficiaryMix

  checkDeadline :: Bool
  checkDeadline = from deadlineMix `contains` txInfoValidRange info

  vestingDatum :: VestingDatumMix
  vestingDatum = case scriptContextScriptInfo ctx of
    SpendingScript _txRef (Just datum) -> case (fromBuiltinData @VestingDatumMix . getDatum) datum of
      Just d  -> d
      Nothing -> traceError "Expected correctly shaped datum"
    _ -> traceError "Expected SpendingScript with datum"

  variables :: (PubKeyHash, POSIXTime)
  variables@(beneficiaryMix, deadlineMix) = case vestingDatum of
    VestingDatum1 b d  -> (b, d)
    VestingDatum2 d b  -> (b, d)

  info :: TxInfo
  info = scriptContextTxInfo ctx

{- ------------------------------------------------------------------------------------------ -}
{- ---------------------------------------- HELPERS ----------------------------------------- -}

compiledVestingValMix :: CompiledCode (BuiltinData -> BuiltinUnit)
compiledVestingValMix = $$(compile [||wrappedVal||])
 where
  wrappedVal :: BuiltinData -> BuiltinUnit
  wrappedVal ctx = check $ vestingValMix (unsafeFromBuiltinData ctx)

serializedVestingValMix :: SerialisedScript
serializedVestingValMix = serialiseCompiledCode compiledVestingValMix
