{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

module Utilities.PlutusTx
  ( wrap
  ) where

import           Plutus.V2.Ledger.Api (UnsafeFromData, unsafeFromBuiltinData)
import           PlutusTx.Prelude     (Bool, BuiltinData, check, ($))

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
  check $ f
      (unsafeFromBuiltinData a)
      (unsafeFromBuiltinData b)
      (unsafeFromBuiltinData c)
