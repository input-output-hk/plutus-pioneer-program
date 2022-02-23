{-# LANGUAGE OverloadedStrings  #-}

module Main
    ( main
    ) where

import Control.Exception          (throwIO)
import Data.String                (IsString (..))
import Network.HTTP.Req
import Plutus.PAB.Webserver.Types (ContractActivationArgs (..))
import System.Environment         (getArgs)
import Text.Printf                (printf)
import Wallet.Emulator.Wallet     (Wallet (..), WalletId (..))
import Wallet.Types               (ContractInstanceId (..))
import Week06.Deploy              (unsafeReadAddress, unsafeReadWalletId)
import Week06.PAB                 (OracleContracts (..))
import Week06.Token               (TokenParams (..))

main :: IO ()
main = do
    [amt', tn', wid', addr'] <- getArgs
    let wid = unsafeReadWalletId wid'
        tp  = TokenParams
                { tpToken   = fromString tn'
                , tpAmount  = read amt'
                , tpAddress = unsafeReadAddress addr'
                }
    printf "minting token for wallet id %s with parameters %s\n" (show wid) $ show tp
    cid <- mintToken wid tp
    printf "minted tokens, contract instance id: %s\n" $ show cid

mintToken :: WalletId -> TokenParams -> IO ContractInstanceId
mintToken wid tp = do
    v <- runReq defaultHttpConfig $ req
        POST
        (http "127.0.0.1" /: "api"  /: "contract" /: "activate")
        (ReqBodyJson caa)
        jsonResponse
        (port 9080)
    let c = responseStatusCode v
    if c == 200
        then return $ responseBody v
        else throwIO $ userError $ printf "ERROR: %d\n" c
  where
    caa :: ContractActivationArgs OracleContracts
    caa = ContractActivationArgs
            { caID = Mint tp
            , caWallet = Just $ Wallet {getWalletId = wid}
            }
