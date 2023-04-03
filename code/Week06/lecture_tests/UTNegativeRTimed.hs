{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import qualified NegativeRTimed as OnChain
import           Plutus.V2.Ledger.Api (PubKeyHash, Value
                                      , TxOut (txOutValue), TxOutRef, POSIXTime)
import           PlutusTx.Prelude     (($), Eq ((==)), (&&), (.))
import           Prelude              (IO, mconcat)
import           Control.Monad        (replicateM, mapM, unless)
import           Plutus.Model         ( ada, adaValue, mustFail,
                                       newUser, payToKey, payToScript, spend, spendScript, submitTx,
                                       testNoErrors, userSpend, valueAt, toV2, logError,
                                       utxoAt, defaultBabbage, Ada(Lovelace),
                                       DatumMode(HashDatum), UserSpend, Tx, Run,
                                       TypedValidator(TypedValidator), waitUntil, validateIn, currentTimeRad )
import           Test.Tasty           ( defaultMain, testGroup )
import PlutusTx.Builtins (mkI, Integer)

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

main :: IO ()
main = do
  defaultMain $ do
    testGroup
      "Testing Homework1"
      [ good "User 1 locks and user 2 takes with R = -42 after dealine succeeds" $ testScript 50 (-42)
      , good "User 1 locks and user 2 takes with R = 0   after dealine succeeds" $ testScript 50 0
      , bad  "User 1 locks and user 2 takes with R = 42  after dealine fails   " $ testScript 50 42
      , bad  "User 1 locks and user 2 takes with R = -42 before dealine fails  " $ testScript 5000 (-42)
      , bad  "User 1 locks and user 2 takes with R = 0   before dealine fails  " $ testScript 5000 0
      , bad  "User 1 locks and user 2 takes with R = 42  before dealine fails  " $ testScript 5000 42
      ]
    where
      bad msg = good msg . mustFail
      good = testNoErrors (adaValue 10_000_000) defaultBabbage

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

waitBeforeConsumingTx :: POSIXTime
waitBeforeConsumingTx = 1000

-- Set many users at once
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 2 $ newUser $ ada (Lovelace 1000)

-- Validator's script
valScript :: TypedValidator datum redeemer
valScript = TypedValidator $ toV2 OnChain.validator 

-- Create transaction that spends "usp" to lock "val" in "giftScript"
lockingTx :: POSIXTime -> UserSpend -> Value -> Tx
lockingTx dl usp val = 
  mconcat
    [ userSpend usp
    , payToScript valScript (HashDatum (OnChain.MkCustomDatum dl)) val
    ]

-- Create transaction that spends "giftRef" to unlock "giftVal" from the "valScript" validator
consumingTx :: POSIXTime -> Integer -> PubKeyHash -> TxOutRef -> Value -> Tx
consumingTx dl redeemer usr giftRef giftVal =
  mconcat
    [ spendScript valScript giftRef (mkI redeemer) (OnChain.MkCustomDatum dl)
    , payToKey usr giftVal
    ]

---------------------------------------------------------------------------------------------------
------------------------------------- TESTING REDEEMERS -------------------------------------------

-- Function to test if both creating and consuming script UTxOs works properly 
testScript :: POSIXTime -> Integer -> Run ()
testScript d r = do
  -- SETUP USERS
  [u1, u2] <- setupUsers
  -- USER 1 LOCKS 100 ADA ("val") IN VALIDATOR
  let val = adaValue 100                    -- Define value to be transfered
  sp <- spend u1 val                        -- Get user's UTXO that we should spend
  submitTx u1 $ lockingTx d sp val            -- User 1 submits "lockingTx" transaction
  -- WAIT FOR A BIT
  waitUntil waitBeforeConsumingTx
  -- USER 2 TAKES "val" FROM VALIDATOR
  utxos <- utxoAt valScript                 -- Query blockchain to get all UTxOs at script
  let [(giftRef, giftOut)] = utxos          -- We know there is only one UTXO (the one we created before)
  ct <- currentTimeRad 100
  tx <- validateIn ct $ consumingTx d r u2 giftRef (txOutValue giftOut)  -- User 2 submits "consumingTx" transaction
  submitTx u2 tx
  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2] <- mapM valueAt [u1, u2]                     -- Get final balances of both users
  unless (v1 == adaValue 900 && v2 == adaValue 1100) $  -- Check if final balances match expected balances
    logError "Final balances are incorrect"