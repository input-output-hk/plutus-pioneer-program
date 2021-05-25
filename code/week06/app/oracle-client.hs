{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main
    ( main
    ) where

import Control.Concurrent
import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString        (ByteString)
import Data.ByteString.Char8  (unpack)
import Data.Proxy             (Proxy (..))
import Data.Text              (pack)
import Data.UUID
import Network.HTTP.Req
import Text.Regex.TDFA

main :: IO ()
main = do
    uuid <- read <$> readFile "oracle.cid"
    putStrLn $ "oracle contract instance id: " ++ show uuid
    go uuid Nothing
  where
    go :: UUID -> Maybe Integer -> IO a
    go uuid m = do
        x <- getExchangeRate
        let y = Just x
        when (m /= y) $
            updateOracle uuid x
        threadDelay 5_000_000
        go uuid y

updateOracle :: UUID -> Integer -> IO ()
updateOracle uuid x = runReq defaultHttpConfig $ do
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "update")
        (ReqBodyJson x)
        (Proxy :: Proxy (JsonResponse ()))
        (port 8080)
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "updated oracle to " ++ show x
        else "error updating oracle"

getExchangeRate :: IO Integer
getExchangeRate = runReq defaultHttpConfig $ do
    v <- req
        GET
        (https "coinmarketcap.com" /: "currencies" /: "cardano")
        NoReqBody
        bsResponse
        mempty
    let priceRegex      = "priceValue___11gHJ\">\\$([\\.0-9]*)" :: ByteString
        (_, _, _, [bs]) = responseBody v =~ priceRegex :: (ByteString, ByteString, ByteString, [ByteString])
        d               = read $ unpack bs :: Double
        x               = round $ 1_000_000 * d
    liftIO $ putStrLn $ "queried exchange rate: " ++ show d
    return x
