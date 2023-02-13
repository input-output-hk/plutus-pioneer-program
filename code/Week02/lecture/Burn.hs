{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Burn where

import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx
import PlutusTx.Prelude
import Cardano.Api

-- Type aliases to make the validator's signature more readable
type BurnDatum = BuiltinData
type BurnRedeemer = BuiltinData
type ScriptContext = BuiltinData

-- This validator always fails
{-# INLINABLE mkBurnValidator #-}
mkBurnValidator :: BurnDatum -> BurnRedeemer -> ScriptContext -> ()
mkBurnValidator _ _ _ = error ()

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mkBurnValidator ||])

serialized :: PlutusScript PlutusScriptV2
serialized = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise $ validator