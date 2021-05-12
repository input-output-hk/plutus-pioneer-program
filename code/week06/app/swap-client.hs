{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
    ( main
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class                  (MonadIO (..))
import Data.Aeson                              (Result (..), fromJSON)
import Data.Monoid                             (Last (..))
import Data.Proxy                              (Proxy (..))
import Data.Text                               (pack)
import Data.UUID
import Ledger.Value                            (flattenValue)
import Network.HTTP.Req
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse (..))
import Plutus.PAB.Webserver.Types
import System.Environment                      (getArgs)
import System.IO
import Text.Read                               (readMaybe)

import Week06.Oracle.PAB                       (OracleContracts)

main :: IO ()
main = do
    [i :: Int] <- map read <$> getArgs
    uuid       <- read <$> readFile ('W' : show i ++ ".cid")
    hSetBuffering stdout NoBuffering
    putStrLn $ "swap contract instance id for Wallet " ++ show i ++ ": " ++ show uuid
    go uuid
  where
    go :: UUID -> IO a
    go uuid = do
        cmd <- readCommand
        case cmd of
            Offer amt -> offer uuid amt
            Retrieve  -> retrieve uuid
            Use       -> use uuid
            Funds     -> getFunds uuid
        go uuid

    readCommand :: IO Command
    readCommand = do
        putStr "enter command (Offer amt, Retrieve, Use or Funds): "
        s <- getLine
        maybe readCommand return $ readMaybe s

data Command = Offer Integer | Retrieve | Use | Funds
    deriving (Show, Read, Eq, Ord)

getFunds :: UUID -> IO ()
getFunds uuid = handle h $ runReq defaultHttpConfig $ do
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "funds")
        (ReqBodyJson ())
        (Proxy :: Proxy (JsonResponse ()))
        (port 8080)
    if responseStatusCode v /= 200
        then liftIO $ putStrLn "error getting funds"
        else do
            w <- req
                GET
                (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "status")
                NoReqBody
                (Proxy :: Proxy (JsonResponse (ContractInstanceClientState OracleContracts)))
                (port 8080)
            liftIO $ putStrLn $ case fromJSON $ observableState $ cicCurrentState $ responseBody w of
                Success (Last (Just f)) -> "funds: " ++ show (flattenValue f)
                _                       -> "error decoding state"
  where
    h :: HttpException -> IO ()
    h _ = threadDelay 1_000_000 >> getFunds uuid

offer :: UUID -> Integer -> IO ()
offer uuid amt = handle h $ runReq defaultHttpConfig $ do
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "offer")
        (ReqBodyJson amt)
        (Proxy :: Proxy (JsonResponse ()))
        (port 8080)
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "offered swap of " ++ show amt ++ " lovelace"
        else "error offering swap"
  where
    h :: HttpException -> IO ()
    h _ = threadDelay 1_000_000 >> offer uuid amt

retrieve :: UUID -> IO ()
retrieve uuid = handle h $ runReq defaultHttpConfig $ do
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "retrieve")
        (ReqBodyJson ())
        (Proxy :: Proxy (JsonResponse ()))
        (port 8080)
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "retrieved swaps"
        else "error retrieving swaps"
  where
    h :: HttpException -> IO ()
    h _ = threadDelay 1_000_000 >> retrieve uuid

use :: UUID -> IO ()
use uuid = handle h $ runReq defaultHttpConfig $ do
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "use")
        (ReqBodyJson ())
        (Proxy :: Proxy (JsonResponse ()))
        (port 8080)
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "used swap"
        else "error using swap"
  where
    h :: HttpException -> IO ()
    h _ = threadDelay 1_000_000 >> use uuid


