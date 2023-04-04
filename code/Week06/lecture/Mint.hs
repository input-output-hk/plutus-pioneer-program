{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Mint where

-- import qualified Cardano.Ledger.Alonzo.Language as C
import           Plutus.V1.Ledger.Value
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified PlutusTx.Prelude          as PlutusTx
import           Utilities                 (wrapPolicy)

fakeMintingPolicy :: TokenName -> MintingPolicy
fakeMintingPolicy mintParams =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||
      \params redeemer ctx ->
        PlutusTx.check (fakeMintingPolicyContract params (unsafeFromBuiltinData redeemer) (unsafeFromBuiltinData ctx))
       ||])
      `PlutusTx.applyCode` PlutusTx.liftCode mintParams

{-# INLINABLE mkWrappedFakePolicy #-}
mkWrappedFakePolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedFakePolicy tn = wrapPolicy (fakeMintingPolicyContract $ PlutusTx.unsafeFromBuiltinData tn)

-- | Can mint new coins if token name equals to fixed tag.
{-# INLINABLE fakeMintingPolicy #-}
fakeMintingPolicyContract :: TokenName -> () -> ScriptContext -> Bool
fakeMintingPolicyContract tag _ ctx =
 valueOf (txInfoMint (scriptContextTxInfo ctx)) (ownCurrencySymbol ctx) tag > 0
