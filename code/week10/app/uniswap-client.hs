{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
    ( main
    ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad                           (forM_, when)
import           Control.Monad.IO.Class                  (MonadIO (..))
import           Data.Aeson                              (Result (..), ToJSON, decode, fromJSON)
import qualified Data.ByteString.Char8                   as B8
import qualified Data.ByteString.Lazy                    as LB
import           Data.Monoid                             (Last (..))
import           Data.Proxy                              (Proxy (..))
import           Data.String                             (IsString (..))
import           Data.Text                               (Text, pack)
import           Data.UUID                               hiding (fromString)
import           Ledger.Value                            (AssetClass (..), CurrencySymbol, Value, flattenValue, TokenName (unTokenName))
import           Network.HTTP.Req
import qualified Plutus.Contracts.Uniswap                as US
import           Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse (..))
import           Plutus.PAB.Webserver.Types
import           System.Environment                      (getArgs)
import           System.Exit                             (exitFailure)
import           Text.Printf                             (printf)
import           Text.Read                               (readMaybe)
import           Wallet.Emulator.Types                   (Wallet (..))

import           Uniswap                                 (cidFile, UniswapContracts)

main :: IO ()
main = do
    w   <- Wallet . read . head <$> getArgs
    cid <- read                 <$> readFile (cidFile w)
    mcs <- decode               <$> LB.readFile "symbol.json"
    case mcs of
        Nothing -> putStrLn "invalid symbol.json" >> exitFailure
        Just cs -> do
            putStrLn $ "cid: " ++ show cid
            putStrLn $ "symbol: " ++ show (cs :: CurrencySymbol)
            go cid cs
  where
    go :: UUID -> CurrencySymbol -> IO a
    go cid cs = do
        cmd <- readCommandIO
        case cmd of
            Funds                    -> getFunds cid
            Pools                    -> getPools cid
            Create amtA tnA amtB tnB -> createPool cid $ toCreateParams cs amtA tnA amtB tnB
            Swap amtA tnA tnB        -> swap cid $ toSwapParams cs amtA tnA tnB
        go cid cs

data Command =
      Funds
    | Pools
    | Create Integer Char Integer Char
    | Swap Integer Char Char
    deriving (Show, Read, Eq, Ord)

readCommandIO :: IO Command
readCommandIO = do
    putStrLn "Enter a command: Funds, Pools, Create amtA tnA amtB tnB"
    s <- getLine
    maybe readCommandIO return $ readMaybe s

toCoin :: CurrencySymbol -> Char -> US.Coin c
toCoin cs tn = US.Coin $ AssetClass (cs, fromString [tn])

toCreateParams :: CurrencySymbol -> Integer -> Char -> Integer -> Char -> US.CreateParams
toCreateParams cs amtA tnA amtB tnB = US.CreateParams (toCoin cs tnA) (toCoin cs tnB) (US.Amount amtA) (US.Amount amtB)

toSwapParams :: CurrencySymbol -> Integer -> Char -> Char -> US.SwapParams
toSwapParams cs amtA tnA tnB = US.SwapParams (toCoin cs tnA) (toCoin cs tnB) (US.Amount amtA) (US.Amount 0)

getFunds :: UUID -> IO ()
getFunds cid = do
    callEndpoint cid "funds" ()
    threadDelay 2_000_000
    go
  where
    go = do
        e <- getStatus cid
        case e of
            Right (US.Funds v) -> showFunds v
            _                  -> go

    showFunds :: Value -> IO ()
    showFunds v = do
        printf "\n                                                 currency symbol                                                         token name          amount\n\n"
        forM_ (flattenValue v) $ \(cs, tn, amt) ->
            printf "%64s %66s %15d\n" (show cs) (show tn) amt
        printf "\n"

getPools :: UUID -> IO ()
getPools cid = do
    callEndpoint cid "pools" ()
    threadDelay 2_000_000
    go
  where
    go = do
        e <- getStatus cid
        case e of
            Right (US.Pools ps) -> putStrLn $ "pools: " ++ show ps
            _                   -> go

createPool :: UUID -> US.CreateParams -> IO ()
createPool cid cp = do
    callEndpoint cid "create" cp
    threadDelay 2_000_000
    go
  where
    go = do
        e <- getStatus cid
        case e of
            Right US.Created -> putStrLn "created"
            Left err'        -> putStrLn $ "error: " ++ show err'
            _                -> go

swap :: UUID -> US.SwapParams -> IO ()
swap cid sp = do
    callEndpoint cid "swap" sp
    threadDelay 2_000_000
    go
  where
    go = do
        e <- getStatus cid
        case e of
            Right US.Swapped -> putStrLn "swapped"
            Left err'        -> putStrLn $ "error: " ++ show err'
            _                -> go

getStatus :: UUID -> IO (Either Text US.UserContractState)
getStatus cid = runReq defaultHttpConfig $ do
    w <- req
        GET
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show cid) /: "status")
        NoReqBody
        (Proxy :: Proxy (JsonResponse (ContractInstanceClientState UniswapContracts)))
        (port 8080)
    case fromJSON $ observableState $ cicCurrentState $ responseBody w of
        Success (Last Nothing)  -> liftIO $ threadDelay 1_000_000 >> getStatus cid
        Success (Last (Just e)) -> return e
        _                       -> liftIO $ ioError $ userError "error decoding state"

callEndpoint :: ToJSON a => UUID -> String -> a -> IO ()
callEndpoint cid name a = handle h $ runReq defaultHttpConfig $ do
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show cid) /: "endpoint" /: pack name)
        (ReqBodyJson a)
        (Proxy :: Proxy (JsonResponse ()))
        (port 8080)
    when (responseStatusCode v /= 200) $
        liftIO $ ioError $ userError $ "error calling endpoint " ++ name
  where
    h :: HttpException -> IO ()
    h = ioError . userError . show
