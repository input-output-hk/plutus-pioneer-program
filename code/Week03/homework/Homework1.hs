{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts ( txSignedBy )
import Plutus.V1.Ledger.Interval
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (traceIfFalse, Bool (..), (||), (&&), (+))
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
mkVestingValidator dat () ctx = 
    traceIfFalse "Benificiary1 did not sign or to late" checkCondition1 || 
    traceIfFalse "Benificiary2 did not sign or is to early" checkCondition2
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        txValidRange :: POSIXTimeRange
        txValidRange  = txInfoValidRange txInfo

        checkCondition1 :: Bool
        checkCondition1 = txSignedBy txInfo (beneficiary1 dat) &&
                          contains (to (deadline dat)) txValidRange

        checkCondition2 :: Bool
        checkCondition2 = txSignedBy txInfo (beneficiary2 dat) &&
                          contains (from (1 + deadline dat)) txValidRange

{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
