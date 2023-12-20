{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Homework1 where

import           Plutus.V2.Ledger.Api 
import           Plutus.V2.Ledger.Contexts
import           Plutus.V1.Ledger.Interval
import qualified PlutusTx
import           PlutusTx.Prelude     (Bool (..), ($), traceIfFalse, (&&))
import           Utilities            (wrapPolicy, writeCodeToFile,
                                       currencySymbol, writePolicyToFile)
import           Prelude              (IO, Show (show))
import           Text.Printf          (printf)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{-# INLINABLE mkDeadlinePolicy #-}
-- This policy should only allow minting (or burning) of tokens if the owner of the specified PubKeyHash
-- has signed the transaction and if the specified deadline has not passed.
mkDeadlinePolicy :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkDeadlinePolicy owner deadline () ctx =
    traceIfFalse "not signed by owner" checkSig &&
    traceIfFalse "too late: deadline has allready passed" checkDeadline
  where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    checkSig :: Bool
    checkSig = txSignedBy txInfo owner

    txValidRange :: POSIXTimeRange
    txValidRange  = txInfoValidRange txInfo

    checkDeadline :: Bool
    checkDeadline = contains (to deadline) txValidRange

{-# INLINABLE mkWrappedDeadlinePolicy #-}
mkWrappedDeadlinePolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedDeadlinePolicy pkh deadline = wrapPolicy $ mkDeadlinePolicy pkh' deadline'
  where 
    pkh' :: PubKeyHash
    pkh' = PlutusTx.unsafeFromBuiltinData pkh
    deadline' :: POSIXTime
    deadline' = PlutusTx.unsafeFromBuiltinData deadline

deadlineCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
deadlineCode = $$(PlutusTx.compile [|| mkWrappedDeadlinePolicy ||])

deadlinePolicy :: PubKeyHash -> POSIXTime -> MintingPolicy
deadlinePolicy pkh deadline = mkMintingPolicyScript $
    deadlineCode
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData pkh)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData deadline)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveDeadlineCode :: IO ()
saveDeadlineCode = writeCodeToFile "assets/deadline.plutus" deadlineCode

saveDeadlinePolicy :: PubKeyHash -> POSIXTime -> IO ()
saveDeadlinePolicy pkh deadline = writePolicyToFile 
                                    (printf "assets/deadline-%s.plutus" $ show pkh) 
                                    $ deadlinePolicy pkh deadline

deadlineCurrencySymbol :: PubKeyHash -> POSIXTime -> CurrencySymbol
deadlineCurrencySymbol pkh deadline = currencySymbol $ deadlinePolicy pkh deadline
