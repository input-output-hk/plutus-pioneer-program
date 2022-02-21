{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}

module Week06.PAB
    ( OracleContracts (..)
    ) where

import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.OpenApi.Schema                 (ToSchema)
import           GHC.Generics                        (Generic)
import           Plutus.PAB.Effects.Contract.Builtin (Empty, HasDefinitions (..), SomeBuiltin (..), endpointsToSchemas)
import           Prettyprinter                       (Pretty (..), viaShow)
import           Ledger                              (Address)

import qualified Week06.Core                         as Oracle
import qualified Week06.Swap                         as Swap
import qualified Week06.Token                        as Token

data OracleContracts = Mint Token.TokenParams | Oracle Oracle.OracleParams | Swap Oracle.Oracle Address
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, ToSchema)

instance Pretty OracleContracts where
    pretty = viaShow

instance HasDefinitions OracleContracts where

    getDefinitions = []

    getContract (Mint tp)     = SomeBuiltin $ Token.mintToken @() @Empty tp
    getContract (Oracle op)   = SomeBuiltin $ Oracle.runOracle op
    getContract (Swap o addr) = SomeBuiltin $ Swap.swap o addr

    getSchema (Mint _)   = endpointsToSchemas @Empty
    getSchema (Oracle _) = endpointsToSchemas @Oracle.OracleSchema
    getSchema (Swap _ _) = endpointsToSchemas @Swap.SwapSchema
