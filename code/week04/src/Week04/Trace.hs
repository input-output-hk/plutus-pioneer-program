{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Week04.Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger
import Ledger.TimeSlot
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

import Week04.Vesting

-- Contract w s e a
-- EmulatorTrace a

test :: IO ()
test = runEmulatorTraceIO myTrace

myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"give" h1 $ GiveParams
        { gpBeneficiary = pubKeyHash $ walletPubKey $ Wallet 2
        , gpDeadline    = slotToBeginPOSIXTime def 20
        , gpAmount      = 10000000
        }
    void $ waitUntilSlot 20
    callEndpoint @"grab" h2 ()
    s <- waitNSlots 1
    Extras.logInfo $ "reached " ++ show s
