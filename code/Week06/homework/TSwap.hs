{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
-- {-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE FlexibleInstances #-}


module Main where

-- import qualified Swap as OnChain
import           PlutusTx.Prelude     (($))
import           Prelude             (IO)
import           Test.Tasty           ( defaultMain, testGroup )

---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------

-- | Test the validator script
main :: IO ()
main = do
  defaultMain $ do
    testGroup
      "Catch double spend with testing"
      [ 
      ]