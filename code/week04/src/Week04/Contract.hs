{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Contract where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import Data.Void                  (Void)
import Plutus.Contract as Contract
    ( logError,
      logInfo,
      tell,
      waitNSlots,
      endpoint,
      handleError,
      throwError,
      type (.\/),
      BlockchainActions,
      Endpoint,
      Contract )
import Plutus.Trace.Emulator as Emulator
    ( activateContractWallet,
      callEndpoint,
      observableState,
      waitNSlots,
      runEmulatorTraceIO,
      EmulatorTrace )
import Wallet.Emulator.Wallet ( Wallet(Wallet) )

-- Contract w s e a
-- EmulatorTrace a

myContract1 :: Contract () BlockchainActions Text ()
myContract1 = do
    void $ Contract.throwError "BOOM!"
    Contract.logInfo @String "Hello from the contract!"

myTrace1 :: EmulatorTrace ()
myTrace1 = void $ activateContractWallet (Wallet 1) myContract1

test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1

myContract2 :: Contract () BlockchainActions Void ()
myContract2 = Contract.handleError
    (\err -> Contract.logError $ "Caught error: " ++ unpack err)
    myContract1

myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (Wallet 1) myContract2

test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2

type MySchema = BlockchainActions .\/ Endpoint "foo" Int

myContract3 :: Contract () MySchema Text ()
myContract3 = do
    n <- endpoint @"foo"
    Contract.logInfo n

myTrace3 :: EmulatorTrace ()
myTrace3 = do
    h <- activateContractWallet (Wallet 1) myContract3
    callEndpoint @"foo" h 42

test3 :: IO ()
test3 = runEmulatorTraceIO myTrace3

myContract4 :: Contract [Int] BlockchainActions Text ()
myContract4 = do
    void $ Contract.waitNSlots 10
    tell [1]
    void $ Contract.waitNSlots 10
    tell [2]
    void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()
myTrace4 = do
    h <- activateContractWallet (Wallet 1) myContract4

    void $ Emulator.waitNSlots 5
    xs <- observableState h
    Extras.logInfo $ show xs

    void $ Emulator.waitNSlots 10
    ys <- observableState h
    Extras.logInfo $ show ys

    void $ Emulator.waitNSlots 10
    zs <- observableState h
    Extras.logInfo $ show zs

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4
