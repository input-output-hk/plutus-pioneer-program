{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Week06.Test where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Data.Text                  (Text)
import           Ledger
import           Ledger.Value               as Value
import           Ledger.Ada                 as Ada
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (IO, Semigroup(..), Show (..))
import           Wallet.Emulator.Wallet

import           Week06.Core
import           Week06.Funds
import           Week06.Swap
import           Week06.Token

assetSymbol :: CurrencySymbol
assetSymbol = "ff"

assetToken :: TokenName
assetToken = "USDT"

testOracle :: IO ()
testOracle = runEmulatorTraceIO' def emCfg myOracleTrace
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig (Left $ Map.fromList [(knownWallet i, v) | i <- [1 .. 10]]) def def

    v :: Value
    v = Ada.lovelaceValueOf                    100_000_000 <>
        Value.singleton assetSymbol assetToken 100_000_000

testToken :: IO ()
testToken = runEmulatorTraceIO myTokenTrace

myTokenTrace :: EmulatorTrace ()
myTokenTrace = do
    let w1 = knownWallet 1
    void $ activateContractWallet w1 $ mintToken TokenParams
        { tpToken   = "USDT"
        , tpAmount  = 100_000
        , tpAddress = mockWalletAddress w1
        }

checkOracle :: Oracle -> Contract () Empty Text a
checkOracle oracle = do
    m <- findOracle oracle
    case m of
        Nothing        -> return ()
        Just (_, _, x) -> Contract.logInfo $ "Oracle value: " ++ show x
    Contract.waitNSlots 1 >> checkOracle oracle

myOracleTrace :: EmulatorTrace ()
myOracleTrace = do
    let op = OracleParams
                { opFees = 1_000_000
                , opSymbol = assetSymbol
                , opToken  = assetToken
                }
    let w1    = knownWallet 1
        w2    = knownWallet 2
        w3    = knownWallet 3
        w4    = knownWallet 4
        w5    = knownWallet 5
        addr1 = mockWalletAddress w1
        addr3 = mockWalletAddress w3
        addr4 = mockWalletAddress w4
        addr5 = mockWalletAddress w5

    h1 <- activateContractWallet (knownWallet 1) $ runOracle op
    void $ Emulator.waitNSlots 1
    oracle <- getOracle h1

    void $ activateContractWallet w2 $ checkOracle oracle

    callEndpoint @"update" h1 1_500_000
    void $ Emulator.waitNSlots 3

    void $ activateContractWallet w1 $ funds' addr1
    void $ activateContractWallet w3 $ funds' addr3
    void $ activateContractWallet w4 $ funds' addr4
    void $ activateContractWallet w5 $ funds' addr5

    h3 <- activateContractWallet w3 $ swap oracle addr3
    h4 <- activateContractWallet w4 $ swap oracle addr4
    h5 <- activateContractWallet w5 $ swap oracle addr5

    callEndpoint @"offer" h3 10_000_000
    callEndpoint @"offer" h4 20_000_000
    void $ Emulator.waitNSlots 3

    callEndpoint @"use" h5 ()
    void $ Emulator.waitNSlots 3

    callEndpoint @"update" h1 1_700_000
    void $ Emulator.waitNSlots 3

    callEndpoint @"use" h5 ()
    void $ Emulator.waitNSlots 3

    callEndpoint @"update" h1 1_800_000
    void $ Emulator.waitNSlots 3

    callEndpoint @"retrieve" h3 ()
    callEndpoint @"retrieve" h4 ()
    void $ Emulator.waitNSlots 3
  where
    getOracle :: ContractHandle (Last Oracle) OracleSchema Text -> EmulatorTrace Oracle
    getOracle h = do
        l <- observableState h
        case l of
            Last Nothing       -> Emulator.waitNSlots 1 >> getOracle h
            Last (Just oracle) -> Extras.logInfo (show oracle) >> return oracle
