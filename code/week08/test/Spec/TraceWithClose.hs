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

module Spec.TraceWithClose
    ( tests
    , runMyTrace
    ) where

import           Control.Lens
import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Ledger
import           Ledger.Value
import           Ledger.Ada                 as Ada
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, String, Show (..))
import           Test.Tasty

import           Week08.TokenSaleWithClose

tests :: TestTree
tests = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "token sale trace"
    (     walletFundsChange (Wallet 1) (Ada.lovelaceValueOf   25_000_000  <> assetClassValue token (-25))
     .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf (-20_000_000) <> assetClassValue token   20)
     .&&. walletFundsChange (Wallet 3) (Ada.lovelaceValueOf (- 5_000_000) <> assetClassValue token    5)
    )
    myTrace

runMyTrace :: IO ()
runMyTrace = runEmulatorTraceIO' def emCfg myTrace

emCfg :: EmulatorConfig
emCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet w, v' w) | w <- [1 .. 3]]
  where
    v :: Value
    v = Ada.lovelaceValueOf 1000_000_000 <> assetClassValue token 1000

    v' :: Integer -> Value
    v' w
        | w == 1    = v <> assetClassValue nft 1
        | otherwise = v

tokenCurrency, nftCurrency :: CurrencySymbol
tokenCurrency = "aa"
nftCurrency   = "01"

tokenName' :: TokenName
tokenName' = "A"

token, nft :: AssetClass
token = AssetClass (tokenCurrency, tokenName')
nft   = AssetClass (nftCurrency, nftName)

myTrace :: EmulatorTrace ()
myTrace = do
    h <- activateContractWallet (Wallet 1) startEndpoint'
    callEndpoint @"start" h (nftCurrency, tokenCurrency, tokenName')
    void $ Emulator.waitNSlots 5
    Last m <- observableState h
    case m of
        Nothing -> Extras.logError @String "error starting token sale"
        Just ts -> do
            Extras.logInfo $ "started token sale " ++ show ts

            h1 <- activateContractWallet (Wallet 1) $ useEndpoints ts
            h2 <- activateContractWallet (Wallet 2) $ useEndpoints ts
            h3 <- activateContractWallet (Wallet 3) $ useEndpoints ts

            callEndpoint @"set price" h1 1_000_000
            void $ Emulator.waitNSlots 5

            callEndpoint @"add tokens" h1 100
            void $ Emulator.waitNSlots 5

            callEndpoint @"buy tokens" h2 20
            void $ Emulator.waitNSlots 5

            callEndpoint @"buy tokens" h3 5
            void $ Emulator.waitNSlots 5

            callEndpoint @"close" h1 ()
--          callEndpoint @"withdraw" h1 (40, 10_000_000)
            void $ Emulator.waitNSlots 5
