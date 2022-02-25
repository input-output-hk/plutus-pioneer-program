{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Week06.PAB
    ( Address
    , TokenContracts (..)
    ) where

import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.OpenApi.Schema                 (ToSchema)
import           GHC.Generics                        (Generic)
import           Ledger                              (Address)
import           Plutus.PAB.Effects.Contract.Builtin (Empty, HasDefinitions (..), SomeBuiltin (..), endpointsToSchemas)
import           Prettyprinter                       (Pretty (..), viaShow)
import           Wallet.Emulator.Wallet              (knownWallet, mockWalletAddress)

import qualified Week06.Monitor                      as Monitor
import qualified Week06.Token.OffChain               as Token

data TokenContracts = Mint Token.TokenParams | Monitor Address
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, ToSchema)

instance Pretty TokenContracts where
    pretty = viaShow

instance HasDefinitions TokenContracts where

    getDefinitions        = [Mint exampleTP, Monitor exampleAddr]

    getContract (Mint tp)      = SomeBuiltin $ Token.mintToken @() @Empty tp
    getContract (Monitor addr) = SomeBuiltin $ Monitor.monitor addr

    getSchema = const $ endpointsToSchemas @Empty

exampleAddr :: Address
exampleAddr = mockWalletAddress $ knownWallet 1

exampleTP :: Token.TokenParams
exampleTP = Token.TokenParams
    { Token.tpAddress = exampleAddr
    , Token.tpAmount  = 123456
    , Token.tpToken   = "PPP"
    }
