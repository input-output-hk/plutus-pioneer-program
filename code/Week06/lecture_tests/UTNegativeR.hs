{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import qualified NegativeR as OnChain
import           Plutus.V2.Ledger.Api (PubKeyHash, Value
                                      , TxOut (txOutValue), TxOutRef)
import           PlutusTx.Prelude     (($), Eq ((==)), (&&), (.))
import           Prelude              (IO, mconcat)
import           Control.Monad        (replicateM, mapM, unless)
import           Plutus.Model         ( ada, adaValue, mustFail,
                                       newUser, payToKey, payToScript, spend, spendScript, submitTx,
                                       testNoErrors, userSpend, valueAt, toV2, logError,
                                       utxoAt, defaultBabbage, Ada(Lovelace),
                                       DatumMode(HashDatum), UserSpend, Tx, Run,
                                       TypedValidator(TypedValidator) )
import           Test.Tasty           ( defaultMain, testGroup )
import PlutusTx.Builtins (mkI, Integer)

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

main :: IO ()
main = do
  defaultMain $ do
    testGroup
      "Testing Homework1"
      [ good "User 1 locks and user 2 takes with Redeemer = -42 succeeds" $ testScript (-42)
      , bad  "User 1 locks and user 2 takes with Redeemer = 42  fails   " $ testScript 42
      , good "User 1 locks and user 2 takes with Redeemer = 0   succeeds" $ testScript 0
      ]
    where
      bad msg = good msg . mustFail
      good = testNoErrors (adaValue 10_000_000) defaultBabbage

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

-- Set many users at once
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 2 $ newUser $ ada (Lovelace 1000)

-- Validator's script
valScript :: TypedValidator datum redeemer
valScript = TypedValidator $ toV2 OnChain.validator 

-- Create transaction that spends "usp" to lock "val" in "giftScript"
lockingTx :: UserSpend -> Value -> Tx
lockingTx usp val = 
  mconcat
    [ userSpend usp
    , payToScript valScript (HashDatum ()) val
    ]

-- Create transaction that spends "giftRef" to unlock "giftVal" from the "valScript" validator
consumingTx :: Integer -> PubKeyHash -> TxOutRef -> Value -> Tx
consumingTx redeemer usr giftRef giftVal =
  mconcat
    [ spendScript valScript giftRef (mkI redeemer) ()
    , payToKey usr giftVal
    ]

---------------------------------------------------------------------------------------------------
------------------------------------- TESTING REDEEMERS -------------------------------------------

-- Function to test if both creating and consuming script UTxOs works properly 
testScript :: Integer -> Run ()
testScript r = do
  -- SETUP USERS
  [u1, u2] <- setupUsers
  -- USER 1 LOCKS 100 ADA ("val") IN VALIDATOR
  let val = adaValue 100                    -- Define value to be transfered
  sp <- spend u1 val                        -- Get user's UTXO that we should spend
  submitTx u1 $ lockingTx sp val            -- User 1 submits "lockingTx" transaction
  -- USER 2 TAKES "val" FROM VALIDATOR
  utxos <- utxoAt valScript                 -- Query blockchain to get all UTxOs at script
  let [(giftRef, giftOut)] = utxos          -- We know there is only one UTXO (the one we created before)
  submitTx u2 $ consumingTx r u2 giftRef (txOutValue giftOut)   -- User 2 submits "consumingTx" transaction
  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2] <- mapM valueAt [u1, u2]
  unless (v1 == adaValue 900 && v2 == adaValue 1100) $
    logError "Final balances are incorrect"