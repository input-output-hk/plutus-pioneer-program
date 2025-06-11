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

module Week06.NegativeRTimed where

import           GHC.Generics                  (Generic)
import           PlutusLedgerApi.Common        (SerialisedScript,
                                                serialiseCompiledCode)
import           PlutusLedgerApi.Data.V3       (FromData (..), POSIXTime, from)
import           PlutusLedgerApi.V1.Interval   (contains)
import           PlutusLedgerApi.V3            (Datum (..), ScriptContext (..),
                                                ScriptInfo (..),
                                                TxInfo (txInfoValidRange),
                                                getRedeemer)
import           PlutusTx                      (BuiltinData, CompiledCode,
                                                UnsafeFromData 
                                                  (unsafeFromBuiltinData),
                                                compile,
                                                makeIsDataSchemaIndexed)
import           PlutusTx.Blueprint            (HasBlueprintDefinition)
import           PlutusTx.Blueprint.Definition (definitionRef)
import           PlutusTx.Bool                 (Bool (..), (&&))
import           PlutusTx.Prelude              (BuiltinUnit, Integer,
                                                Maybe (..), Ord ((<=)), check,
                                                traceError, traceIfFalse, ($),
                                                (.))

{- ------------------------------------------------------------------------------------------ -}
{- ----------------------------------------- TYPES ------------------------------------------ -}

newtype CustomDatum = MkCustomDatum {deadline :: POSIXTime}
  deriving stock (Generic)
  deriving newtype (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''CustomDatum [('MkCustomDatum, 0)]

{- ------------------------------------------------------------------------------------------ -}
{- --------------------------------------- VALIDATOR ---------------------------------------- -}

{-# INLINEABLE negativeRTimedVal #-}
negativeRTimedVal :: ScriptContext -> Bool
negativeRTimedVal ctx =
  traceIfFalse "expected a negative redeemer" (red <= 0)
    && traceIfFalse "deadline not reached" deadlineReached
 where
  info :: TxInfo
  info = scriptContextTxInfo ctx

  deadlineReached :: Bool
  deadlineReached = contains (from (deadline dat)) $ txInfoValidRange info

  dat :: CustomDatum
  dat = case scriptContextScriptInfo ctx of
    SpendingScript _txRef (Just datum) -> 
      case (fromBuiltinData @CustomDatum . getDatum) datum of
        Just d  -> d
        Nothing -> traceError "Expected correctly shaped datum"
    _ -> traceError "Expected SpendingScript with datum"

  red :: Integer
  red = case (fromBuiltinData @Integer 
              . getRedeemer 
              . scriptContextRedeemer) ctx of
          Just r  -> r
          Nothing -> traceError "Expected redeemer"

{- ------------------------------------------------------------------------------------------ -}
{- ---------------------------------------- HELPERS ----------------------------------------- -}

compiledVal :: CompiledCode (BuiltinData -> BuiltinUnit)
compiledVal = $$(compile [||wrappedVal||])
 where
  wrappedVal :: BuiltinData -> PlutusTx.Prelude.BuiltinUnit
  wrappedVal ctx = PlutusTx.Prelude.check PlutusTx.Prelude.$ 
                     negativeRTimedVal (unsafeFromBuiltinData ctx)

serializedNegativeRTimedVal :: SerialisedScript
serializedNegativeRTimedVal = serialiseCompiledCode compiledVal
