{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Monad (replicateM)
import Plutus.Model
import Plutus.V1.Ledger.Api
import Prelude
import Test.Tasty 
import Data.Functor (void)

-- alocate 3 users with 1000 lovelaces each
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 3 $ newUser $ ada (Lovelace 1000)

main :: IO ()
main = do
  defaultMain $ do
    testGroup "All tests"
      [ 
        --tests defaultBabbage
        -- TODO: Add tests
      ]

{-
tests :: MockConfig -> TestTree
tests cfg = do
  testGroup
    "Test simple user scripts"
    [ good "Simple spend" simpleSpend
    , bad "Not enough funds" notEnoughFunds
    ]
  where
    good = check True
    bad = check False
-}


simpleSpend :: Run Bool
simpleSpend = do
  users <- setupUsers                -- create 3 users and assign each 1000 lovelaces
  let [u1, u2, u3] = users           -- give names for users
  sendValue u1 (adaValue 100) u2     -- send 100 lovelaces from user 1 to user 2
  sendValue u2 (adaValue 100) u3     -- send 100 lovelaces from user 2 to user 3
  isOk <- noErrors                   -- check that all TXs were accepted without errors
  vals <- mapM valueAt users         -- read user values
  pure $ and                         -- check test predicate
    [ isOk
     , vals == fmap adaValue [900, 1000, 1100]
    ]

notEnoughFunds :: Run Bool
notEnoughFunds = do
  users <- setupUsers
  let [u1, u2, _u3] = users
  void $ sendValue u1 (adaValue 10000) u2
  noErrors