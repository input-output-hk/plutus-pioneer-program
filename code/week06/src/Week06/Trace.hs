{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Week06.Trace
    ( testToken
    ) where

import           Control.Monad              hiding (fmap)
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (IO)
import           Wallet.Emulator.Wallet

import           Week06.Token.OffChain

testToken :: IO ()
testToken = runEmulatorTraceIO tokenTrace

tokenTrace :: EmulatorTrace ()
tokenTrace = do
    let w1 = knownWallet 1
    void $ activateContractWallet w1 $ void $ mintToken @() @Empty TokenParams
        { tpToken   = "USDT"
        , tpAmount  = 100_000
        , tpAddress = mockWalletAddress w1
        }
