{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main
    ( main
    ) where

import Control.Concurrent                      (threadDelay)
import Control.Exception                       (throwIO)
import Control.Monad                           (when)
import Data.Aeson                              (FromJSON (..))
import Data.Aeson.Types                        (parseMaybe)
import Data.Maybe                              (fromMaybe)
import Data.Monoid                             (Last (..))
import Data.Text                               (pack)
import Network.HTTP.Req
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse (..))
import Plutus.PAB.Webserver.Types              (ContractInstanceClientState (..))
import Plutus.V1.Ledger.Value                  (Value, flattenValue)
import System.Environment                      (getArgs)
import Text.Printf                             (printf)
import Wallet.Emulator.Wallet                  (WalletId (..))
import Week06.PAB                              (Address, TokenContracts (..))
import Wallet.Types                            (ContractInstanceId (..))
import Week06.Utils                            (cidToString, contractActivationArgs, unsafeReadAddress, unsafeReadWalletId)

main :: IO ()
main = do
    [wid', addr'] <- getArgs
    let wid  = unsafeReadWalletId wid'
        addr = unsafeReadAddress addr'
    printf "monitoring address %s on wallet %s\n" (show addr) $ show wid
    cid <- startMonitor wid addr
    printf "started monitor-process with contract id %s\n\n" $ cidToString cid
    go cid mempty
  where
    go :: ContractInstanceId -> Value -> IO a
    go cid v = do
        cic <- getMonitorState cid
        let v' = fromMaybe v $ observedValue cic
        when (v' /= v) $
            printf "%s\n\n" $ show $ flattenValue v'
        threadDelay 1_000_000
        go cid v'

startMonitor :: WalletId -> Address -> IO ContractInstanceId
startMonitor wid addr = do
    v <- runReq defaultHttpConfig $ req
        POST
        (http "127.0.0.1" /: "api"  /: "contract" /: "activate")
        (ReqBodyJson $ contractActivationArgs wid $ Monitor addr)
        jsonResponse
        (port 9080)
    let c = responseStatusCode v
    when (c /= 200) $
        throwIO $ userError $ printf "ERROR: %d\n" c
    return $ responseBody v

getMonitorState :: ContractInstanceId -> IO (ContractInstanceClientState TokenContracts)
getMonitorState cid = do
    v <- runReq defaultHttpConfig $ req
        GET
        (http "127.0.0.1" /: "api"  /: "contract" /: "instance" /: pack (cidToString cid) /: "status")
        NoReqBody
        jsonResponse
        (port 9080)
    let c = responseStatusCode v
    when (c /= 200) $
        throwIO $ userError $ printf "ERROR: %d\n" c
    return $ responseBody v

observedValue :: ContractInstanceClientState TokenContracts -> Maybe Value
observedValue cic = do
    Last mv <- parseMaybe parseJSON $ observableState $ cicCurrentState cic
    mv
