{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Vesting where

import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.ByteString.Short as BSS
import qualified Plutus.V2.Ledger.Api  as PlutusV2
import           PlutusTx              (BuiltinData, compile)
import           PlutusTx.Prelude      (($), (.))

{-# INLINABLE mkVestingValidator #-}
mkVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkVestingValidator _ _ _ = ()

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mkVestingValidator ||])

serialized :: PlutusScript PlutusScriptV2
serialized = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise $ validator
