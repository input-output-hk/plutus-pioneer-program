{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Utils (writePlutusFile, wrap) where

import Plutus.V2.Ledger.Api qualified as PlutusV2
import Plutus.V2.Ledger.Api (UnsafeFromData, unsafeFromBuiltinData) 
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import Cardano.Api.Shelley (PlutusScript (..))
import Cardano.Api
import PlutusTx.Prelude
import Codec.Serialise (serialise)
import Prelude (IO, putStrLn, print, FilePath)

-- Serialize script
serializeScript :: PlutusV2.Validator -> PlutusScript PlutusScriptV2
serializeScript = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise

-- Create file with compiled Plutus script
writePlutusFile :: FilePath -> PlutusV2.Validator -> IO ()
writePlutusFile filePath validator =
  writeFileTextEnvelope filePath Nothing (serializeScript validator) >>= \case
    Left err -> print $ displayError err
    Right _ -> putStrLn $ "Compiled Plutus script at: " ++ filePath

{-# INLINABLE wrap #-}
wrap ::
  forall a b c.
  ( UnsafeFromData a,
    UnsafeFromData b,
    UnsafeFromData c
  ) =>
  (a -> b -> c -> Bool) ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
wrap f a b c =
  check
    ( f
        (unsafeFromBuiltinData a)
        (unsafeFromBuiltinData b)
        (unsafeFromBuiltinData c)
    )
