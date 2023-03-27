{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Free where

import           Plutus.V2.Ledger.Api (BuiltinData, CurrencySymbol,
                                       MintingPolicy, ScriptContext,
                                       mkMintingPolicyScript)
import qualified PlutusTx
import           PlutusTx.Prelude     (Bool (True))
import           Prelude              (IO)
import           Utilities            (currencySymbol, wrapPolicy,
                                       writePolicyToFile)

{-# INLINABLE mkFreePolicy #-}
mkFreePolicy :: () -> ScriptContext -> Bool
mkFreePolicy () _ = True

mkWrappedFreePolicy :: BuiltinData -> BuiltinData -> ()
mkWrappedFreePolicy = wrapPolicy mkFreePolicy

freePolicy :: MintingPolicy
freePolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| mkWrappedFreePolicy ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveFreePolicy :: IO ()
saveFreePolicy = writePolicyToFile "assets/free.plutus" freePolicy

freeCurrencySymbol :: CurrencySymbol
freeCurrencySymbol = currencySymbol freePolicy
