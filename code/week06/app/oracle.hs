{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Main
    ( main
    ) where

import           Control.Monad                           (void)
import           Control.Monad.Freer                     (Eff, Member, interpret, type (~>))
import           Control.Monad.Freer.Error               (Error)
import           Control.Monad.Freer.Extras.Log          (LogMsg)
import           Control.Monad.IO.Class                  (MonadIO (..))
import           Data.Aeson                              (FromJSON, ToJSON)
import           Data.Text.Prettyprint.Doc               (Pretty (..), viaShow)
import           GHC.Generics                            (Generic)
import           Plutus.Contract                         (BlockchainActions, ContractError)
import           Plutus.PAB.Effects.Contract             (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin     (Builtin, SomeBuiltin (..), type (.\\), endpointsToSchemas, handleBuiltin)
import           Plutus.PAB.Effects.ContractTest.Uniswap as US
import           Plutus.PAB.Monitoring.PABLogMsg         (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                    (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                    as Simulator
import           Plutus.PAB.Types                        (PABError (..))
import qualified Plutus.PAB.Webserver.Server             as PAB.Server

import           Wallet.Emulator.Types                   (Wallet (..))

import qualified Week06.Oracle.Core                      as Oracle
import qualified Week06.Oracle.Swap                      as Oracle

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin OracleContracts) "Starting Oracle PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    void $ liftIO getLine
    shutdown

data OracleContracts = Oracle | Swap Oracle.Oracle
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance Pretty OracleContracts where
    pretty = viaShow

oracleParams :: Oracle.OracleParams
oracleParams = Oracle.OracleParams
    { Oracle.opFees   = 1_000_000
    , Oracle.opSymbol = "ff"
    , Oracle.opToken  = "USDT"
    }

handleOracleContracts ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin OracleContracts))) effs
    )
    => ContractEffect (Builtin OracleContracts)
    ~> Eff effs
handleOracleContracts = handleBuiltin getSchema getContract where
    getSchema = \case
        Oracle -> endpointsToSchemas @(Oracle.OracleSchema .\\ BlockchainActions)
        Swap _ -> endpointsToSchemas @(Oracle.SwapSchema   .\\ BlockchainActions)
    getContract = \case
        Oracle      -> SomeBuiltin $ Oracle.runOracle oracleParams
        Swap oracle -> SomeBuiltin $ Oracle.swap oracle

handlers :: SimulatorEffectHandlers (Builtin OracleContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin OracleContracts) []
    $ interpret handleOracleContracts
