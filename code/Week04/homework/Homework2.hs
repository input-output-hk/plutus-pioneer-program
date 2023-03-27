{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Homework2 where

import           Plutus.V1.Ledger.Interval (contains)
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx                  (applyCode, compile, liftCode)
import           PlutusTx.Prelude          (Bool (..), traceIfFalse, (&&), (.))
import           Prelude                   (IO)
import           Utilities                 (wrapValidator, writeValidatorToFile)

---------------------------------------------------------------------------------------------------
------------------------------------------ PROMPT -------------------------------------------------

{-
1- Figure out what this (already finished) validator does using all the tools at your disposal.
2- Write the off-chain code necessary to cover all possible interactions with the validator using
   the off-chain tool of your choosing.

HINT: If you get stuck, take a look at Week03's lecture
-}

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{-# INLINABLE mkParameterizedMisteryValidator #-}
mkParameterizedMisteryValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkParameterizedMisteryValidator beneficiary deadline () ctx =
    traceIfFalse "not signed by beneficiary" checkSig &&
    traceIfFalse "deadline has not passed yet" checkDeadline
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        txValidRange :: POSIXTimeRange
        txValidRange  = txInfoValidRange txInfo

        checkSig :: Bool
        checkSig = txSignedBy txInfo beneficiary

        checkDeadline :: Bool
        checkDeadline = contains (from deadline) txValidRange

{-# INLINABLE  mkWrappedParameterizedMisteryValidator #-}
mkWrappedParameterizedMisteryValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedMisteryValidator = wrapValidator . mkParameterizedMisteryValidator

validator :: PubKeyHash -> Validator
validator beneficiary = mkValidatorScript ($$(compile [|| mkWrappedParameterizedMisteryValidator ||]) `applyCode` liftCode beneficiary)


---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: PubKeyHash -> IO ()
saveVal = writeValidatorToFile "./assets/parameterized-Mistery.plutus" . validator
