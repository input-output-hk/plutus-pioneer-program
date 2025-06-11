{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Week02.Validators where

import GHC.Generics                  (Generic)
import PlutusLedgerApi.Common        (FromData (fromBuiltinData), 
                                      SerialisedScript,
                                      serialiseCompiledCode)
import PlutusLedgerApi.Data.V3       (Redeemer (getRedeemer),
                                      ScriptContext (..))
import PlutusTx                      (BuiltinData, CompiledCode,
                                      UnsafeFromData (unsafeFromBuiltinData),
                                      compile, makeIsDataSchemaIndexed)
import PlutusTx.Builtins             (unsafeDataAsI)
import PlutusTx.Bool                 (Bool (..))
import PlutusTx.Prelude              (BuiltinUnit, Eq (..), Integer,
                                      Maybe (..), check, traceError,
                                      traceIfFalse, ($), otherwise,
                                      (.))
import PlutusTx.Blueprint            (HasBlueprintDefinition)
import PlutusTx.Blueprint.Definition (definitionRef)
import qualified PlutusTx.Builtins.Internal as BI (
                                      BuiltinList, BuiltinInteger, 
                                      head, snd, tail, unitval,
                                      unsafeDataAsConstr)

{- NOTE 
   If importing ScriptContent from the PlutusLedgerApi.V3 module instead 
   of the PlutusLedgerApi.Data.V3, CBOR sizes are around 10% larger. 
-}

{- ----------------------------------------------------------------------------------------- -}
{- --------------------------------- Always True validator --------------------------------- -}

{-# INLINEABLE mkGiftValidator #-}
mkGiftValidator :: BuiltinData -> BuiltinUnit
mkGiftValidator _ctx = check True

compiledMkGiftValidator :: CompiledCode (BuiltinData -> BuiltinUnit)
compiledMkGiftValidator = $$(compile [||mkGiftValidator||])

serializedMkGiftValidator :: SerialisedScript
serializedMkGiftValidator = serialiseCompiledCode compiledMkGiftValidator

{- ------------------------------------------------------------------------------------------ -}
{- --------------------------------- Always False validator --------------------------------- -}

{-# INLINEABLE mkBurnValidator #-}
mkBurnValidator :: BuiltinData -> BuiltinUnit
mkBurnValidator _ctx = traceError "it burns!!!"

compiledMkBurnValidator :: CompiledCode (BuiltinData -> BuiltinUnit)
compiledMkBurnValidator = $$(compile [||mkBurnValidator||])

serializedMkBurnValidator :: SerialisedScript
serializedMkBurnValidator = serialiseCompiledCode compiledMkBurnValidator

{- ------------------------------------------------------------------------------------------ -}
{- ----------------------------- 42 validator untyped large CBOR ---------------------------- -}

{-# INLINEABLE mk42ValidatorLarge #-}
mk42ValidatorLarge :: BuiltinData -> BuiltinUnit
mk42ValidatorLarge ctx 
    | r == 42   = BI.unitval
    | otherwise = traceError "Expected 42 integer redeemer"
 where
  ctxTyped = case fromBuiltinData ctx of
    Just @ScriptContext c -> c
    Nothing -> traceError "ScriptContext could not be converted from BuiltinData" 
  r = case fromBuiltinData $ getRedeemer (scriptContextRedeemer ctxTyped) of
    Just @Integer n -> n
    Nothing -> traceError "Redeemer is not a number"  

compiledMk42ValidatorLarge :: CompiledCode (BuiltinData -> BuiltinUnit)
compiledMk42ValidatorLarge = $$(compile [||mk42ValidatorLarge||])

serializedMk42ValidatorLarge :: SerialisedScript
serializedMk42ValidatorLarge = serialiseCompiledCode compiledMk42ValidatorLarge

{- ------------------------------------------------------------------------------------------ -}
{- ----------------------------- 42 validator untyped small CBOR ---------------------------- -}

{-# INLINEABLE mk42ValidatorSmall #-}
mk42ValidatorSmall :: BuiltinData -> BuiltinUnit
mk42ValidatorSmall ctx 
    | redeemerInt == 42 = BI.unitval
    | otherwise         = traceError "Expected 42 integer redeemer"
 where
    -- Lazily decode script context up to redeemer; 
    -- is less expensive and results in much smaller tx size 
    constrArgs :: BuiltinData -> BI.BuiltinList BuiltinData
    constrArgs = BI.snd . BI.unsafeDataAsConstr

    scriptContextBL :: BI.BuiltinList BuiltinData
    scriptContextBL = constrArgs ctx

    redeemerBD :: BuiltinData
    redeemerBD = BI.head . BI.tail $ scriptContextBL

    redeemerInt :: BI.BuiltinInteger 
    redeemerInt = unsafeDataAsI redeemerBD

compiledMk42ValidatorSmall :: CompiledCode (BuiltinData -> BuiltinUnit)
compiledMk42ValidatorSmall = $$(compile [||mk42ValidatorSmall||])

serializedMk42ValidatorSmall :: SerialisedScript
serializedMk42ValidatorSmall = serialiseCompiledCode compiledMk42ValidatorSmall

{- ------------------------------------------------------------------------------------------ -}
{- ----------------------------------- 42 validator typed ----------------------------------- -}

{-# INLINEABLE mk42TypedValidator #-}
mk42TypedValidator :: ScriptContext -> Bool
mk42TypedValidator ctx = traceIfFalse "Redeemer is a number different than 42" $ 42 == r
 where
  r = case fromBuiltinData $ getRedeemer (scriptContextRedeemer ctx) of
    Just @Integer n -> n
    Nothing -> traceError "Redeemer is not a number"

compiledMk42TypedValidator :: CompiledCode (BuiltinData -> BuiltinUnit)
compiledMk42TypedValidator = $$(compile [||wrappedVal||])
 where
  wrappedVal :: BuiltinData -> BuiltinUnit
  wrappedVal ctx = check $ mk42TypedValidator (unsafeFromBuiltinData ctx)

serializedMk42TypedValidator :: SerialisedScript
serializedMk42TypedValidator = serialiseCompiledCode compiledMk42TypedValidator

{- ------------------------------------------------------------------------------------------ -}
{- -------------------------------- 42 validator custom type -------------------------------- -}
 
-- Custom data types for our redeemer 
data MySillyRedeemer = MkMySillyRedeemer Integer 
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''MySillyRedeemer [('MkMySillyRedeemer, 0)]

{-# INLINEABLE mk42CustomValidator #-}
mk42CustomValidator :: ScriptContext -> Bool
mk42CustomValidator ctx = traceIfFalse "Redeemer is a number different than 42" $ 42 == r
 where 
   r = case fromBuiltinData @MySillyRedeemer . getRedeemer $ scriptContextRedeemer ctx of
     Just (MkMySillyRedeemer rInt) -> rInt
     Nothing -> traceError "Redeemer is not of MySillyRedeemer type."

compiledMk42CustomValidator :: CompiledCode (BuiltinData -> BuiltinUnit)
compiledMk42CustomValidator = $$(compile [||wrappedVal||])
 where
  wrappedVal :: BuiltinData -> BuiltinUnit
  wrappedVal ctx = check $ mk42CustomValidator (unsafeFromBuiltinData ctx)

serializedMk42CustomValidator :: SerialisedScript
serializedMk42CustomValidator = serialiseCompiledCode compiledMk42CustomValidator

{- ------------------------------------------------------------------------------------------ -}
{- -------------------------- Datum 42 validator untyped small CBOR ------------------------- -} 

{-# INLINEABLE read42ValidatorSmall #-}
read42ValidatorSmall :: BuiltinData -> BuiltinUnit
read42ValidatorSmall ctx 
    | datumInt == 42 = BI.unitval
    | otherwise      = traceError "Datum is a number different than 42" 
 where
    -- Lazily decode script context up to datum  
    constrArgs :: BuiltinData -> BI.BuiltinList BuiltinData
    constrArgs = BI.snd . BI.unsafeDataAsConstr

    scriptInfoBD :: BuiltinData
    scriptInfoBD = BI.head . BI.tail . BI.tail $ constrArgs ctx

    maybeDatumBD :: BuiltinData
    maybeDatumBD = BI.head . BI.tail $ constrArgs scriptInfoBD 

    datumBD :: BuiltinData
    datumBD = BI.head $ constrArgs maybeDatumBD

    datumInt :: BI.BuiltinInteger 
    datumInt = unsafeDataAsI datumBD 

compiledRead42ValidatorSmall :: CompiledCode (BuiltinData -> BuiltinUnit)
compiledRead42ValidatorSmall = $$(compile [||read42ValidatorSmall||])

serializedRead42ValidatorSmall :: SerialisedScript
serializedRead42ValidatorSmall = serialiseCompiledCode compiledRead42ValidatorSmall
