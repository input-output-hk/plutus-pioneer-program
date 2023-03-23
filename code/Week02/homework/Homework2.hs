-- NARENDRA DAS date:- 23.03.2023

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Homework2 where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx             (unstableMakeIsData, compile)
import           PlutusTx.Prelude     (Bool, (/=), BuiltinData)
import           Prelude              ()
import           Utilities            (wrap)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data MyRedeemer = MyRedeemer
    { flag1 :: Bool
    , flag2 :: Bool
    }

PlutusTx.unstableMakeIsData ''MyRedeemer

{-# INLINABLE mkValidator #-}
-- Create a validator that unlocks the funds if MyRedemeer's flags are different
mkValidator :: () -> MyRedeemer -> PlutusV2.ScriptContext -> Bool
mkValidator _ MyRedeemer{flag1 = flag1', flag2 = flag2'} _ = flag1' /= flag2'

wrappedVal :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedVal = wrap mkValidator

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrappedVal ||])

