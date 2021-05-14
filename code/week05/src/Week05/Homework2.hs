{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.Homework2 where

import           Control.Monad          hiding (fmap)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract hiding (when)
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (Semigroup (..))
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

{-# INLINABLE mkPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
mkPolicy :: TxOutRef -> ScriptContext -> Bool
mkPolicy oref ctx = True -- FIX ME!

policy :: TxOutRef -> Scripts.MonetaryPolicy
policy oref = undefined -- IMPLEMENT ME!

curSymbol :: TxOutRef -> CurrencySymbol
curSymbol = undefined -- IMPLEMENT ME!

type NFTSchema =
    BlockchainActions
        .\/ Endpoint "mint" ()

mint :: Contract w NFTSchema Text ()
mint = undefined -- IMPLEMENT ME!

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >> mint

mkSchemaDefinitions ''NFTSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"mint" h1 ()
    callEndpoint @"mint" h2 ()
    void $ Emulator.waitNSlots 1
