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

module Week08.Staking where

import           PlutusLedgerApi.Common   (SerialisedScript,
                                           serialiseCompiledCode)
import           PlutusLedgerApi.Data.V3  (Address)
import           PlutusLedgerApi.V1.Value (valueOf)
import           PlutusLedgerApi.V3       (Credential, Lovelace (getLovelace),
                                           ScriptContext (..), ScriptInfo (..),
                                           TxInfo (..), TxOut (..), adaSymbol,
                                           adaToken, txInfoWdrl)
import           PlutusTx                 (BuiltinData, CompiledCode,
                                           UnsafeFromData (unsafeFromBuiltinData),
                                           compile)
import           PlutusTx.AssocMap        (lookup)
import           PlutusTx.Bool            (Bool (..), otherwise)
import           PlutusTx.Prelude         (BuiltinUnit, Eq (..), Integer,
                                           Maybe (..), Ord ((>=)), check, foldl,
                                           traceError, traceIfFalse, ($), (*),
                                           (+))

{- ------------------------------------------------------------------------------------------ -}
{- --------------------------------------- VALIDATOR ---------------------------------------- -}

{-# INLINEABLE stakeVal #-}
stakeVal :: Address -> ScriptContext -> Bool
stakeVal addr ctx = case scriptContextScriptInfo ctx of
  CertifyingScript _ _ -> True
  RewardingScript cred -> traceIfFalse "insufficient reward sharing" $ 2 * paidToAddress >= amount cred
  _ -> False
 where
  info :: TxInfo
  info = scriptContextTxInfo ctx

  amount :: Credential -> Integer
  amount cred = case lookup cred $ txInfoWdrl info of
    Just amt -> getLovelace amt
    Nothing  -> traceError "withdrawal not found"

  paidToAddress :: Integer
  paidToAddress = foldl f 0 $ txInfoOutputs info
   where
    f :: Integer -> TxOut -> Integer
    f n o
      | txOutAddress o == addr = n + valueOf (txOutValue o) adaSymbol adaToken
      | otherwise = n

{- ------------------------------------------------------------------------------------------ -}
{- ---------------------------------------- HELPERS ----------------------------------------- -}

compiledVal :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
compiledVal = $$(compile [||wrappedVal||])
 where
  wrappedVal :: BuiltinData -> BuiltinData -> BuiltinUnit
  wrappedVal addr ctx = PlutusTx.Prelude.check $ stakeVal (unsafeFromBuiltinData addr) (unsafeFromBuiltinData ctx)

serializedStakingVal :: SerialisedScript
serializedStakingVal = serialiseCompiledCode compiledVal
