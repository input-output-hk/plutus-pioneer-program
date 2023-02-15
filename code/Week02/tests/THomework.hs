{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Monad (replicateM)
import Plutus.Model
import Plutus.V1.Ledger.Api
import Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Data.Functor (void)

-- alocate 3 users with 1000 lovelaces each
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 3 $ newUser $ ada (Lovelace 1000)

main :: IO ()
main = do
  defaultMain $ do
    testGroup "Homework tests"
      [ 
        -- tests defaultBabbage
      ]

-- tests :: MockConfig -> TestTree
-- tests cfg = do
--   testGroup
--     "Test simple user scripts"
--     [ good "Simple spend" simpleSpend
--     , bad "Not enough funds" notEnoughFunds
--     ]
--   where
--     good = check True
--     bad = check False
--     check res msg act = testCase msg $ fst (runMock act (initMock cfg $ adaValue 10_000_000)) @?= res

-- simpleSpend :: Run Bool
-- simpleSpend = do
--   users <- setupUsers
--   let [u1, u2, u3] = users
--       val = adaValue 100
--   checkBalance (gives u1 val u2) $ sendValue u1 val u2
--   checkBalance (gives u2 val u3) $ sendValue u2 val u3
--   isOk <- noErrors
--   vals <- mapM valueAt users
--   pure $ isOk && vals == fmap adaValue [900, 1000, 1100]

-- notEnoughFunds :: Run Bool
-- notEnoughFunds = do
--   users <- setupUsers
--   let [u1, u2, _u3] = users
--   void $ sendValue u1 (adaValue 10000) u2
--   noErrors
