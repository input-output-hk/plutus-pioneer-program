{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Data.Maybe
import           Plutus.V1.Ledger.Interval (contains)
import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext (scriptContextTxInfo), Validator,
                                       TxInfo (txInfoValidRange), to, from, mkValidatorScript,
                                       mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool, traceIfFalse, ($), (&&), (||), (+))
import           Utilities            (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator dat () ctx = (traceIfFalse "benef1 has not signed the tx" beneficiary1Sign && traceIfFalse "deadline passed" beforeDeadline) ||
                                (traceIfFalse "benef2 has not signet the tx" beneficiary2Sign && traceIfFalse "deadline not reached" afterDeadline)

    where 
        info :: TxInfo
        info = scriptContextTxInfo ctx

        beneficiary1Sign :: Bool 
        beneficiary1Sign = txSignedBy info $ beneficiary1 dat

        beneficiary2Sign :: Bool 
        beneficiary2Sign = txSignedBy info $ beneficiary2 dat

        beforeDeadline :: Bool 
        beforeDeadline = contains (to $ deadline dat) $ txInfoValidRange info

        afterDeadline :: Bool 
        afterDeadline = contains (from $ 1 + deadline dat) $ txInfoValidRange info

{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
