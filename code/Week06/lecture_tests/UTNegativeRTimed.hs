{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import           Control.Monad        (mapM, replicateM, unless)
import qualified NegativeRTimed       as OnChain
import           Plutus.Model         (Ada (Lovelace), DatumMode (HashDatum),
                                       Run, Tx, TypedValidator (TypedValidator),
                                       UserSpend, ada, adaValue, currentTimeRad,
                                       defaultBabbage, logError, mustFail,
                                       newUser, payToKey, payToScript, spend,
                                       spendScript, submitTx, testNoErrors,
                                       toV2, userSpend, utxoAt, validateIn,
                                       valueAt, waitUntil)
import           Plutus.V2.Ledger.Api (POSIXTime, PubKeyHash,
                                       TxOut (txOutValue), TxOutRef, Value)
import           PlutusTx.Builtins    (Integer, mkI)
import           PlutusTx.Prelude     (Eq ((==)), ($), (&&), (.))
import           Prelude              (IO, mconcat)
import           Test.Tasty           (defaultMain, testGroup)

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

main :: IO ()
main = defaultMain $ do
    testGroup
      "Testing validator with some sensible values"
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

-- Create transaction that spends "usp" to lock "val" in "valScript"
lockingTx :: POSIXTime -> UserSpend -> Value -> Tx
lockingTx dl usp val =
  mconcat
    [ userSpend usp
    , payToScript valScript (HashDatum (OnChain.MkCustomDatum dl)) val
    ]

-- Create transaction that spends "ref" to unlock "val" from the "valScript" validator
consumingTx :: POSIXTime -> Integer -> PubKeyHash -> TxOutRef -> Value -> Tx
consumingTx dl redeemer usr ref val =
  mconcat
    [ spendScript valScript ref (mkI redeemer) (OnChain.MkCustomDatum dl)
    , payToKey usr val
    ]

---------------------------------------------------------------------------------------------------
------------------------------------- TESTING REDEEMERS -------------------------------------------

-- Function to test if both creating and consuming script UTxOs works properly
testScript :: POSIXTime -> Integer -> Run ()
testScript d r = do
  -- SETUP USERS
  [u1, u2] <- setupUsers
  -- USER 1 LOCKS 100 Lovelaces ("val") IN VALIDATOR
  let val = adaValue 100                    -- Define value to be transfered
  sp <- spend u1 val                        -- Get user's UTXO that we should spend
  submitTx u1 $ lockingTx d sp val          -- User 1 submits "lockingTx" transaction
  -- WAIT FOR A BIT
  waitUntil waitBeforeConsumingTx
  -- USER 2 TAKES "val" FROM VALIDATOR
  utxos <- utxoAt valScript                 -- Query blockchain to get all UTxOs at script
  let [(ref, out)] = utxos                  -- We know there is only one UTXO (the one we created before)
  ct <- currentTimeRad 100                  -- Create time interval with equal radius around current time
  tx <- validateIn ct $ consumingTx d r u2 ref (txOutValue out)  -- Build Tx
  submitTx u2 tx                            -- User 2 submits "consumingTx" transaction
  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2] <- mapM valueAt [u1, u2]                     -- Get final balances of both users
  unless (v1 == adaValue 900 && v2 == adaValue 1100) $  -- Check if final balances match expected balances
    logError "Final balances are incorrect"
