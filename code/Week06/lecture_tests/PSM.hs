{-# LANGUAGE NumericUnderscores #-}

module Main where

import           Prelude
import           Test.Tasty           (defaultMain, testGroup)

import           Control.Monad        (replicateM)
import           Plutus.Model         (Ada (Lovelace), Run, ada, adaValue,
                                       defaultBabbage, mustFail, newUser,
                                       noErrors, sendValue, testNoErrors,
                                       valueAt)
import           Plutus.V1.Ledger.Api (PubKeyHash)

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

main :: IO ()
main = defaultMain $ do
    testGroup
      "Test simple user transactions"
      [ good "Simple spend" simpleSpend
      , bad  "Not enough funds" notEnoughFunds
      ]
      where
        bad msg = good msg . mustFail
        good = testNoErrors (adaValue 10_000_000) defaultBabbage

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

-- Set many users at once
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 3 $ newUser $ ada (Lovelace 1000)

---------------------------------------------------------------------------------------------------
------------------------------------- TESTING TRANSACTIONS ----------------------------------------

-- Function to test that a simple transaction works
simpleSpend :: Run Bool
simpleSpend = do
    users <- setupUsers                -- Create 3 users and assign each 1000 lovelaces
    let [u1, u2, u3] = users           -- Give names to individual users
    sendValue u1 (adaValue 100) u2     -- Send 100 lovelaces from user 1 to user 2
    sendValue u2 (adaValue 100) u3     -- Send 100 lovelaces from user 2 to user 3
    isOk <- noErrors                   -- Check that all TXs were accepted without errors
    vals <- mapM valueAt users         -- Read user values
    return $ isOk &&                     -- Check isOk and that all users have correct values
           (vals == fmap adaValue [900, 1000, 1100])

-- Function to test that a transaction fails if there are not enough funds
notEnoughFunds :: Run Bool
notEnoughFunds = do
  users <- setupUsers               -- Create 3 users and assign each 1000 lovelaces
  let [u1, u2, _u3] = users         -- Give names to individual users
  sendValue u1 (adaValue 10000) u2  -- Send 10.000 lovelaces from user 1 to user 2
  noErrors  -- Check that all TXs were accepted without errors (should fail)
