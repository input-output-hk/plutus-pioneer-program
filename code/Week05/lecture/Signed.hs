{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Signed where

import           Plutus.V2.Ledger.Api      (BuiltinData, CurrencySymbol,
                                            MintingPolicy, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            mkMintingPolicyScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import qualified PlutusTx
import           PlutusTx.Prelude          (Bool, traceIfFalse, ($), (.))
import           Prelude                   (IO, Show (show))
import           Text.Printf               (printf)
import           Utilities                 (currencySymbol, wrapPolicy,
                                            writeCodeToFile, writePolicyToFile)

{-# INLINABLE mkSignedPolicy #-}
mkSignedPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkSignedPolicy pkh () ctx = traceIfFalse "missing signature" $ txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE mkWrappedSignedPolicy #-}
mkWrappedSignedPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedSignedPolicy pkh = wrapPolicy (mkSignedPolicy $ PlutusTx.unsafeFromBuiltinData pkh)

signedCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
signedCode = $$(PlutusTx.compile [|| mkWrappedSignedPolicy ||])

signedPolicy :: PubKeyHash -> MintingPolicy
signedPolicy pkh = mkMintingPolicyScript $ signedCode `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData pkh)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveSignedCode :: IO ()
saveSignedCode = writeCodeToFile "assets/signed.plutus" signedCode

saveSignedPolicy :: PubKeyHash -> IO ()
saveSignedPolicy pkh = writePolicyToFile (printf "assets/signed-%s.plutus" $ show pkh) $ signedPolicy pkh

signedCurrencySymbol :: PubKeyHash -> CurrencySymbol
signedCurrencySymbol = currencySymbol . signedPolicy
