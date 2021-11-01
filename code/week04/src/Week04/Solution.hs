{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Solution where

import Control.Monad          (forever)
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
    { ppRecipient :: PubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = forever
    $ handleError (\err -> Contract.logInfo $ "caught error: " ++ unpack err)
    $ awaitPromise
    $ endpoint @"pay" $ \pp -> do
        let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
        void $ submitTx tx

payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace x y = do
    h <- activateContractWallet (knownWallet 1) payContract
    let pkh = pubKeyHash $ walletPubKey $ knownWallet 2
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
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000000000 2000000
