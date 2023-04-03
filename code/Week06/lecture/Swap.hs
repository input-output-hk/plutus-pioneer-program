{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Swap where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx             (compile, unstableMakeIsData)
import PlutusTx.Builtins ( BuiltinData , Integer )
import PlutusTx.Prelude ( Bool (..), (==) )
import Plutus.V2.Ledger.Api (ScriptContext)
import Plutus.V2.Ledger.Contexts (valuePaidTo)
import Utilities (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data DatumSwap = DatumSwap
  { beneficiary :: PlutusV2.PubKeyHash
  , price       :: Integer
  }
PlutusTx.unstableMakeIsData ''DatumSwap

{-# INLINABLE mkValidator #-}
mkValidator :: DatumSwap -> () -> ScriptContext -> Bool
mkValidator ds _ ctx = outputToBeneficiary
  where
    outputToBeneficiary = valuePaidTo txInfo (beneficiary ds) == PlutusV2.singleton PlutusV2.adaSymbol PlutusV2.adaToken (price ds)

    txInfo = PlutusV2.scriptContextTxInfo ctx


{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator mkValidator


validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mkWrappedValidator ||])
