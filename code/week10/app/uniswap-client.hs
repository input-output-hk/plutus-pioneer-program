{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
    ( main
    ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad                           (forever)
import           Control.Monad.IO.Class                  (MonadIO (..))
import           Data.Aeson                              (Result (..), decode, fromJSON)
import qualified Data.ByteString.Lazy as LB
import           Data.Monoid                             (Last (..))
import           Data.Proxy                              (Proxy (..))
import           Data.Text                               (Text, pack)
import           Data.UUID
import           Ledger.Value                            (CurrencySymbol, flattenValue)
import           Network.HTTP.Req
import qualified Plutus.Contracts.Uniswap                as US
import           Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse (..))
import           Plutus.PAB.Webserver.Types
import           System.Environment                      (getArgs)
import           System.Exit                             (exitFailure)
import           System.IO
import           Text.Read                               (readMaybe)
import           Wallet.Emulator.Types                   (Wallet (..))

import           Uniswap                                 (cidFile, UniswapContracts)

main :: IO ()
main = do
    w   <- Wallet . read . head <$> getArgs
    cid <- read                 <$> readFile (cidFile w)
    mus <- decode               <$> LB.readFile "uniswap.json"
    mcs <- decode               <$> LB.readFile "symbol.json"
    case (mus, mcs) of
        (Just us, Just cs) -> do
            putStrLn $ "cid: " ++ show cid
            putStrLn $ "uniswap: " ++ show (us :: US.Uniswap)
            putStrLn $ "symbol: " ++ show (cs :: CurrencySymbol)
            go cid
        _ -> putStrLn "invalid uniswap.json and/or symbol.json" >> exitFailure
  where
    go :: UUID -> IO a
    go cid = do
        cmd <- readCommandIO
        case cmd of
            Funds -> getFunds cid
            Pools -> getPools cid
        go cid

data Command = Funds | Pools
    deriving (Show, Read, Eq, Ord)

readCommandIO :: IO Command
readCommandIO = do
    putStrLn "Enter a command: Funds, Pools"
    s <- getLine
    maybe readCommandIO return $ readMaybe s

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
            liftIO $ threadDelay 2_000_000
            w <- req
                GET
                (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "status")
                NoReqBody
                (Proxy :: Proxy (JsonResponse (ContractInstanceClientState UniswapContracts)))
                (port 8080)
            liftIO $ putStrLn $ case fromJSON $ observableState $ cicCurrentState $ responseBody w of
                Success (Last (Just (Right (US.Funds f)))) -> "funds: " ++ show (flattenValue f)
                Success (Last (Just (Left e)))             -> "error: " ++ show (e :: Text)
                _                                          -> "error decoding state"
  where
    h :: HttpException -> IO ()
    h _ = threadDelay 1_000_000 >> getFunds uuid

getPools :: UUID -> IO ()
getPools uuid = handle h $ runReq defaultHttpConfig $ do
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "pools")
        (ReqBodyJson ())
        (Proxy :: Proxy (JsonResponse ()))
        (port 8080)
    if responseStatusCode v /= 200
        then liftIO $ putStrLn "error getting funds"
        else do
            liftIO $ threadDelay 2_000_000
            w <- req
                GET
                (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "status")
                NoReqBody
                (Proxy :: Proxy (JsonResponse (ContractInstanceClientState UniswapContracts)))
                (port 8080)
            liftIO $ putStrLn $ case fromJSON $ observableState $ cicCurrentState $ responseBody w of
                Success (Last (Just (Right (US.Pools ps)))) -> "pools: " ++ show ps
                Success (Last (Just (Left e)))              -> "error: " ++ show (e :: Text)
                _                                           -> "error decoding state"
  where
    h :: HttpException -> IO ()
    h _ = threadDelay 1_000_000 >> getFunds uuid
{-
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


-}
