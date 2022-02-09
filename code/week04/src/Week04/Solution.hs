{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Week04.Solution where

import Data.Aeson             (FromJSON, ToJSON)
import Data.Functor           (void)
import Data.Text              (Text, unpack)
import GHC.Generics           (Generic)
import Ledger
import Ledger.Ada             as Ada
import Ledger.Constraints     as Constraints
import Plutus.Contract        as Contract
import Plutus.Trace.Emulator  as Emulator
import Wallet.Emulator.Wallet

data PayParams = PayParams
    { ppRecipient :: PaymentPubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- awaitPromise $ endpoint @"pay" return
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    handleError (\err -> Contract.logInfo $ "caught error: " ++ unpack err) $ void $ submitTx tx
    payContract

payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace x y = do
    h <- activateContractWallet (knownWallet 1) payContract
    let pkh = mockWalletPaymentPubKeyHash $ knownWallet 2
    callEndpoint @"pay" h $ PayParams
        { ppRecipient = pkh
        , ppLovelace  = x
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"pay" h $ PayParams
        { ppRecipient = pkh
        , ppLovelace  = y
        }
    void $ Emulator.waitNSlots 1

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 10_000_000 20_000_000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000_000_000 20_000_000
