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

module FortyTwo where

import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx
import PlutusTx.Builtins as Builtins
import PlutusTx.Prelude
import Cardano.Api

-- Type aliases to make the validator's signature more readable
type GiftDatum = BuiltinData
type GiftRedeemer = BuiltinData
type ScriptContext = BuiltinData

-- This validator succeeds only if the redeemer is 42
{-# INLINABLE mkGiftValidator #-}
mkGiftValidator :: GiftDatum -> GiftRedeemer -> ScriptContext -> ()
mkGiftValidator _ r _
    | r == Builtins.mkI 42 = ()
    | otherwise            = error ()

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mkGiftValidator ||])

serialized :: PlutusScript PlutusScriptV2
serialized = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise $ validator