{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import           Control.Monad           (mapM, replicateM)
import qualified NegativeR               as OnChain
import           Plutus.Model            (Ada (Lovelace), DatumMode (HashDatum),
                                          Run, Tx,
                                          TypedValidator (TypedValidator),
                                          UserSpend, ada, adaValue,
                                          defaultBabbage, initMock, mustFail,
                                          newUser, payToKey, payToScript,
                                          runMock, spend, spendScript, submitTx,
                                          toV2, userSpend, utxoAt, valueAt)
import           Plutus.V2.Ledger.Api    (PubKeyHash, TxOut (txOutValue),
                                          TxOutRef, Value)
import           PlutusTx.Builtins       (Integer, mkI)
import           PlutusTx.Prelude        (Bool (..), Eq ((==)), Ord ((<=)),
                                          return, ($), (&&), (.))
import           Prelude                 (IO, Num ((+), (-)), Ord ((<), (>)),
                                          mconcat)
import           Test.QuickCheck         (Property, Testable (property),
                                          collect, (==>))
import           Test.QuickCheck.Monadic (PropertyM, assert, monadic, run)
import           Test.Tasty              (defaultMain, testGroup)
import           Test.Tasty.QuickCheck   as QC (testProperty)

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

-- | Make Run an instance of Testable so we can use it with QuickCheck
instance Testable a => Testable (Run a) where
  property rp = let (a,_) = runMock rp $ initMock defaultBabbage (adaValue 10_000_000) in property a

-- | Test the validator script
main :: IO ()
main = do
  defaultMain $ do
    testGroup
      "Testing script properties regarding redeemer values"
      [ testProperty "All values succeed" prop_successAllValues
      , testProperty "Positive redeemers fail" prop_failIfPositive
      , testProperty "Negative redeemers succeed" prop_successIfNegative
      -- , testProperty "Negative values succeed (showing tested redeemers)" prop_successIfNegative'
      ]

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

-- Set many users at once
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 2 $ newUser $ ada (Lovelace 1000)

-- Validator's script
valScript :: TypedValidator datum redeemer
valScript = TypedValidator $ toV2 OnChain.validator

-- Create transaction that spends "usp" to lock "val" in the "valScript" validator
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
------------------------------------- TESTING VALUES ----------------------------------------------

-- | The validator should have the property that only negative values succeed
prop_successAllValues :: Integer -> Property
prop_successAllValues v = (v > 0) ==> monadic property $ checkValues v

-- | Check that the expected and real balances match after using the validator with different values
checkValues :: Integer -> PropertyM Run ()
checkValues value = do
  balancesMatch <- run $ testValue value
  assert balancesMatch

-- Function to test if both creating an consuming script UTxOs works properly
testValue :: Integer -> Run Bool
testValue v = do
  -- SETUP USERS
  [u1, u2] <- setupUsers
  -- USER 1 LOCKS 100 ADA ("val") IN VALIDATOR
  let val = adaValue v                      -- Define value to be transfered
  sp <- spend u1 val                        -- Get user's UTXO that we should spend
  submitTx u1 $ lockingTx sp val            -- User 1 submits "lockingTx" transaction
  -- USER 2 TAKES "val" FROM VALIDATOR
  utxos <- utxoAt valScript                 -- Query blockchain to get all UTxOs at script
  let [(oRef, oOut)] = utxos                -- We know there is only one UTXO (the one we created before)
  submitTx u2 $ consumingTx 0 u2 oRef (txOutValue oOut)           -- User 2 submits "consumingTx" transaction
  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2] <- mapM valueAt [u1, u2]                               -- Get final balances
  return $ v1 == adaValue (1000 - v) && v2 == adaValue (1000 + v) -- Check that final balances match expected balances

---------------------------------------------------------------------------------------------------
------------------------------------- TESTING REDEEMERS -------------------------------------------

-- | The validator should have the property that all positive values fail
prop_failIfPositive :: Integer -> Property
prop_failIfPositive r = (r > 0) ==> monadic property $ checkRedeemers False r

-- | The validator should have the property that only negative values succeed
prop_successIfNegative :: Integer -> Property
prop_successIfNegative r = (r < 0) ==> monadic property $ checkRedeemers True r

-- | Same as prop_successIfNeg but collecting the redeemer value for further analysis
prop_successIfNegative' :: Integer -> Property
prop_successIfNegative' r = (r <= 0) ==> collect r $ monadic property $ checkRedeemers True r

-- | Check that the expected and real balances match after using the validator with different redeemers
checkRedeemers :: Bool -> Integer -> PropertyM Run ()
checkRedeemers shouldConsume redeemer = do
  balancesMatch <- run $ testRedeemer shouldConsume redeemer
  assert balancesMatch

-- Function to test if both creating an consuming script UTxOs works properly
testRedeemer :: Bool -> Integer -> Run Bool
testRedeemer shouldConsume redeemer = do
  -- SETUP USERS
  [u1, u2] <- setupUsers
  -- USER 1 LOCKS 100 ADA ("val") IN VALIDATOR
  let val = adaValue 100                    -- Define value to be transfered
  sp <- spend u1 val                        -- Get user's UTXOs that we should spend
  submitTx u1 $ lockingTx sp val            -- User 1 submits "lockingTx" transaction
  -- USER 2 TAKES "val" FROM VALIDATOR
  utxos <- utxoAt valScript                 -- Query blockchain to get all UTxOs at script
  let [(oRef, oOut)] = utxos                -- We know there is only one UTXO (the one we created before)
      tx = consumingTx redeemer u2 oRef (txOutValue oOut)                  -- Define transaction to be submitted
      v2Expected = if shouldConsume then adaValue 1100 else adaValue 1000  -- Define expected balance for user 2
  if shouldConsume then submitTx u2 tx else mustFail . submitTx u2 $ tx    -- User 2 submits "consumingTx" transaction
  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2] <- mapM valueAt [u1, u2]               -- Get final balances
  return $ v1 == adaValue 900 && v2 == v2Expected -- Check if final balances match expected balances
