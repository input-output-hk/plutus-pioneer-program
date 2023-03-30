{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Homework1 where

import           Plutus.V2.Ledger.Api (BuiltinData, MintingPolicy, POSIXTime,
                                       PubKeyHash, ScriptContext,
                                       mkMintingPolicyScript, POSIXTimeRange)
import           Plutus.V2.Ledger.Contexts
import           Plutus.V1.Ledger.Interval
import qualified PlutusTx
import           PlutusTx.Prelude     (Bool (..), ($), traceIfFalse, (&&))
import           Utilities            (wrapPolicy)

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
mkWrappedDeadlinePolicy :: PubKeyHash -> POSIXTime -> BuiltinData -> BuiltinData -> ()
mkWrappedDeadlinePolicy pkh deadline = wrapPolicy $ mkDeadlinePolicy pkh deadline

deadlinePolicy :: PubKeyHash -> POSIXTime -> MintingPolicy
deadlinePolicy pkh deadline = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkWrappedDeadlinePolicy ||])
        `PlutusTx.applyCode` PlutusTx.liftCode pkh
        `PlutusTx.applyCode` PlutusTx.liftCode deadline
  