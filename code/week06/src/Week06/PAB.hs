{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}

module Week06.PAB
    ( Address
    , TokenParams (..)
    , TokenContracts (..)
    ) where

import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.OpenApi.Schema                 (ToSchema)
import           GHC.Generics                        (Generic)
import           Ledger                              (Address)
import           Plutus.PAB.Effects.Contract.Builtin (Empty, HasDefinitions (..), SomeBuiltin (..), endpointsToSchemas)
import           Prettyprinter                       (Pretty (..), viaShow)

import qualified Week06.Monitor                      as Monitor
import qualified Week06.Token.OffChain               as Token
import Week06.Token.OffChain (TokenParams(TokenParams))

data TokenContracts = Mint Token.TokenParams | Monitor Address
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, ToSchema)

instance Pretty TokenContracts where
    pretty = viaShow

instance HasDefinitions TokenContracts where

    getDefinitions        = []

    getContract (Mint tp)      = SomeBuiltin $ Token.mintToken @() @Empty tp
    getContract (Monitor addr) = SomeBuiltin $ Monitor.monitor addr

    getSchema (Mint _)    = endpointsToSchemas @Empty
    getSchema (Monitor _) = endpointsToSchemas @Empty
