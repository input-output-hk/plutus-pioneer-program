module Week04.Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Data.Text                  (Text)
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

test :: IO ()
test = runEmulatorTraceIO myTrace

myTrace :: EmulatorTrace ()
myTrace = do
    h  <- activateContractWallet (Wallet 1) myContract
    void $ Emulator.waitNSlots 1
    xs <- Emulator.observableState h
    Extras.logInfo $ show xs


myContract :: Contract [Int] BlockchainActions Text ()
myContract = do
    Contract.logInfo "logging..."
    tell [1]
    void $ Contract.waitNSlots 10
    myContract
