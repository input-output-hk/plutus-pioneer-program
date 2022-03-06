{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Spec.Trace
    ( tests
    , testCoverage
    , runMyTrace
    ) where

import           Control.Exception                            (try)
import           Control.Lens
import           Control.Monad                                hiding (fmap)
import           Control.Monad.Freer.Extras                   as Extras
import           Data.Default                                 (Default (..))
import           Data.IORef
import qualified Data.Map                                     as Map
import           Data.Monoid                                  (Last (..))
import           Ledger
import           Ledger.Value
import           Ledger.Ada                                   as Ada
import           Plutus.Contract.Test
import           Plutus.Contract.Test.Coverage
import           Plutus.Trace.Emulator                        as Emulator
import qualified PlutusTx.Prelude                             as Plutus
import           System.Exit                                  (ExitCode (..))
import           Test.Tasty
import qualified Test.Tasty.HUnit                             as HUnit

import           Plutus.Contract.Test.Coverage.ReportCoverage (writeCoverageReport)
import           Week08.TokenSale

tests :: TestTree
tests = checkPredicateOptions
    myOptions
    "token sale trace"
    myPredicate
    myTrace

testCoverage :: IO ()
testCoverage = do
    cref <- newCoverageRef
    e <- try $ defaultMain $ checkPredicateOptionsCoverage
        myOptions
        "token sale trace"
        cref
        myPredicate
        myTrace
    case e of
        Left (c :: ExitCode) -> do
            putStrLn $ "Tasty exited with: " ++ show c
            report <- readCoverageRef cref
            writeCoverageReport "TokenSaleTrace" tsCovIdx report
        Right () -> putStrLn $ "unexpected tasty result"

myOptions :: CheckOptions
myOptions = defaultCheckOptions & emulatorConfig .~ emCfg

myPredicate :: TracePredicate
myPredicate =
    walletFundsChange w1 (Ada.lovelaceValueOf   10_000_000  <> assetClassValue token (-60) <> Plutus.negate (toValue minAdaTxOut)) .&&.
    walletFundsChange w2 (Ada.lovelaceValueOf (-20_000_000) <> assetClassValue token   20)                                         .&&.
    walletFundsChange w3 (Ada.lovelaceValueOf (- 5_000_000) <> assetClassValue token    5)

runMyTrace :: IO ()
runMyTrace = runEmulatorTraceIO' def emCfg myTrace

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(knownWallet w, v) | w <- [1 .. 3]]) def def
  where
    v :: Value
    v = Ada.lovelaceValueOf 1_000_000_000 <> assetClassValue token 1000

currency :: CurrencySymbol
currency = "aa"

name :: TokenName
name = "A"

token :: AssetClass
token = AssetClass (currency, name)

myTrace :: EmulatorTrace ()
myTrace = do
    h <- activateContractWallet w1 startEndpoint
    callEndpoint @"start" h (currency, name)
    void $ Emulator.waitNSlots 5
    Last m <- observableState h
    case m of
        Nothing -> Extras.logError @String "error starting token sale"
        Just ts -> do
            Extras.logInfo $ "started token sale " ++ show ts

            h1 <- activateContractWallet w1 $ useEndpoints ts
            h2 <- activateContractWallet w2 $ useEndpoints ts
            h3 <- activateContractWallet w3 $ useEndpoints ts

            callEndpoint @"set price" h1 1_000_000
            void $ Emulator.waitNSlots 5

            callEndpoint @"add tokens" h1 100
            void $ Emulator.waitNSlots 5

            callEndpoint @"buy tokens" h2 20
            void $ Emulator.waitNSlots 5

            callEndpoint @"buy tokens" h3 5
            void $ Emulator.waitNSlots 5

            callEndpoint @"withdraw" h1 (40, 10_000_000)
            void $ Emulator.waitNSlots 5

checkPredicateOptionsCoverage :: CheckOptions
                              -> String
                              -> CoverageRef
                              -> TracePredicate
                              -> EmulatorTrace ()
                              -> TestTree
checkPredicateOptionsCoverage options nm (CoverageRef ioref) predicate action =
    HUnit.testCaseSteps nm $ \step -> do
        checkPredicateInner options predicate action step (HUnit.assertBool nm) (\rep -> modifyIORef ioref (rep<>))
