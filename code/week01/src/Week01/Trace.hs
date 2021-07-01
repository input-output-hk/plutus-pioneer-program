{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NumericUnderscores    #-}

module Week01.Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Ledger
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet
import Ledger.Value         as Value
import Ledger.TimeSlot
import Ledger.Ada                 as Ada
import qualified Data.Map                   as Map
import           Data.Default               (Default (..))

import Week01.EnglishAuction

assetSymbol :: CurrencySymbol
assetSymbol = "66"

assetToken :: TokenName
assetToken = "T"

-- Contract w s e a
-- EmulatorTrace a

test :: IO ()
test = runEmulatorTraceIO' def emCfg myTrace
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig $ Left $ Map.fromList [
                              (Wallet 1, v <> Value.singleton assetSymbol assetToken 1)
                            , (Wallet 2, v)
                            , (Wallet 3, v)]

    v :: Value
    v = Ada.lovelaceValueOf 100_000_000

myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    h3 <- activateContractWallet (Wallet 3) endpoints
    callEndpoint @"start" h1 $ StartParams
        { spDeadline  = slotToPOSIXTime 10
        , spMinBid    = 3
        , spCurrency  = assetSymbol
        , spToken     = assetToken
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"bid" h2 $ BidParams
        { bpCurrency = assetSymbol
        , bpToken    = assetToken
        , bpBid      = 3
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"bid" h3 $ BidParams
        { bpCurrency = assetSymbol
        , bpToken    = assetToken
        , bpBid      = 5
        }
    s <- waitUntilSlot 20
    Extras.logInfo $ "reached slot " ++ show s
    callEndpoint @"close" h1 $ CloseParams
        {
            cpCurrency = assetSymbol
          , cpToken    = assetToken
        }


