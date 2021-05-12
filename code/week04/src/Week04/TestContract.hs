{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Week04.Contract where

import Control.Monad.Freer.Extras as Extras
import Data.Functor                  (void)
import Data.Text                     (Text, unpack)
import Data.Void                     (Void)
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

-- Contract w s e a
-- EmulatorTrace a

myContract1' :: Contract () BlockchainActions Text ()
myContract1' = Contract.logInfo @String "Hello from the contract lokita!"

myTrace1' :: EmulatorTrace ()
myTrace1' = void $ activateContractWallet (Wallet 1) myContract1'

test1' :: IO ()
test1' = runEmulatorTraceIO myTrace1'

myContract2' :: Contract () BlockchainActions Void ()
myContract2' = Contract.handleError
    (\err -> Contract.logError $ "Caugth error: " ++ unpack err)
    myContract1' 

myTrace2' :: EmulatorTrace ()
myTrace2' = void $ activateContractWallet (Wallet 1) myContract2' 

test2' :: IO ()
test2' = runEmulatorTraceIO myTrace2' 
