{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week06.Oracle.Test where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Data.Text                  (Text)
import           Ledger
import           Ledger.Value               as Value
import           Ledger.Ada                 as Ada
import           Plutus.Contract            as Contract hiding (when)
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (Semigroup(..))
import           Wallet.Emulator.Wallet

import           Week06.Oracle.Core
import           Week06.Oracle.Funds
import           Week06.Oracle.Swap

assetSymbol :: CurrencySymbol
assetSymbol = "ff"

assetToken :: TokenName
assetToken = "USDT"

test :: IO ()
test = runEmulatorTraceIO' def emCfg myTrace
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet i, v) | i <- [1 .. 10]]

    v :: Value
    v = Ada.lovelaceValueOf                    100_000_000 <>
        Value.singleton assetSymbol assetToken 100_000_000

checkOracle :: Oracle -> Contract () BlockchainActions Text a
checkOracle oracle = do
    m <- findOracle oracle
    case m of
        Nothing        -> return ()
        Just (_, _, x) -> Contract.logInfo $ "Oracle value: " ++ show x
    Contract.waitNSlots 1 >> checkOracle oracle

myTrace :: EmulatorTrace ()
myTrace = do
    let op = OracleParams
                { opFees = 1_000_000
                , opSymbol = assetSymbol
                , opToken  = assetToken
                }

    h1 <- activateContractWallet (Wallet 1) $ runOracle op
    void $ Emulator.waitNSlots 1
    oracle <- getOracle h1

    void $ activateContractWallet (Wallet 2) $ checkOracle oracle

    callEndpoint @"update" h1 1_500_000
    void $ Emulator.waitNSlots 3

    void $ activateContractWallet (Wallet 1) ownFunds'
    void $ activateContractWallet (Wallet 3) ownFunds'
    void $ activateContractWallet (Wallet 4) ownFunds'
    void $ activateContractWallet (Wallet 5) ownFunds'

    h3 <- activateContractWallet (Wallet 3) $ swap oracle
    h4 <- activateContractWallet (Wallet 4) $ swap oracle
    h5 <- activateContractWallet (Wallet 5) $ swap oracle

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
