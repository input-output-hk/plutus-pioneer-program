{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import           Control.Monad        (mapM, replicateM, unless)
import qualified NegativeR            as OnChain
import           Plutus.Model         (Ada (Lovelace), DatumMode (HashDatum),
                                       Run, Tx, TypedValidator (TypedValidator),
                                       UserSpend, ada, adaValue, defaultBabbage,
                                       logError, mustFail, newUser, payToKey,
                                       payToScript, spend, spendScript,
                                       submitTx, testNoErrors, toV2, userSpend,
                                       utxoAt, valueAt)
import           Plutus.V2.Ledger.Api (PubKeyHash, TxOut (txOutValue), TxOutRef,
                                       Value)
import           PlutusTx.Builtins    (Integer, mkI)
import           PlutusTx.Prelude     (Eq ((==)), ($), (&&), (.))
import           Prelude              (IO, mconcat)
import           Test.Tasty           (defaultMain, testGroup)

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

-- Create transaction that spends "usp" to lock "val" in "valScript"
lockingTx :: UserSpend -> Value -> Tx
lockingTx usp val =
  mconcat
    [ userSpend usp
    , payToScript valScript (HashDatum ()) val
    ]

-- Create transaction that spends "oRef" to unlock "val" from the "valScript" validator
consumingTx :: Integer -> PubKeyHash -> TxOutRef -> Value -> Tx
consumingTx redeemer usr oRef val =
  mconcat
    [ spendScript valScript oRef (mkI redeemer) ()
    , payToKey usr val
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
  let [(oRef, oOut)] = utxos                -- We know there is only one UTXO (the one we created before)
  submitTx u2 $ consumingTx r u2 oRef (txOutValue oOut) -- User 2 submits "consumingTx" transaction
  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2] <- mapM valueAt [u1, u2]                     -- Get final balances of both users
  unless (v1 == adaValue 900 && v2 == adaValue 1100) $  -- Check if final balances match expected balances
    logError "Final balances are incorrect"
