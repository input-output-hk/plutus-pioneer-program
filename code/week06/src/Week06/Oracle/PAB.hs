{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Week06.Oracle.PAB
    ( OracleContracts (..)
    , usdt
    , wallets
    ) where

import           Control.Monad                       (forM_, when)
import           Data.OpenApi.Schema                 as OpenApi
import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import           Data.Text                           (Text, pack)
import           Data.Monoid                         (Last (..))
import           GHC.Generics                        (Generic)
import           Ledger
import           Ledger.Constraints
import qualified Ledger.Value                        as Value
import           Plutus.Contract
import           Plutus.PAB.Effects.Contract.Builtin (SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import qualified Plutus.Contracts.Currency           as Currency
import           Wallet.Emulator.Types               (Wallet (..), knownWallet, walletPubKey)
import qualified Week06.Oracle.Core                  as Oracle
import qualified Week06.Oracle.Swap                  as Oracle

data OracleContracts = Init | Oracle CurrencySymbol | Swap Oracle.Oracle
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty OracleContracts where
    pretty = viaShow

instance Builtin.HasDefinitions OracleContracts where
    getDefinitions = [Init]
    getSchema = \case
        Init     -> Builtin.endpointsToSchemas @Empty
        Oracle _ -> Builtin.endpointsToSchemas @Oracle.OracleSchema
        Swap _   -> Builtin.endpointsToSchemas @Oracle.SwapSchema
    getContract = \case
        Init        -> SomeBuiltin   initContract
        Oracle cs   -> SomeBuiltin $ Oracle.runOracle $ oracleParams cs
        Swap oracle -> SomeBuiltin $ Oracle.swap oracle

oracleParams :: CurrencySymbol -> Oracle.OracleParams
oracleParams cs = Oracle.OracleParams
    { Oracle.opFees   = 1_000_000
    , Oracle.opSymbol = cs
    , Oracle.opToken  = usdt
    }

usdt :: TokenName
usdt = "USDT"

initContract :: Contract (Last CurrencySymbol) Empty Text ()
initContract = do
    ownPK <- pubKeyHash <$> ownPubKey
    cur   <-
        mapError (pack . show)
        (Currency.mintContract ownPK [(usdt, fromIntegral (length wallets) * amount)]
        :: Contract (Last CurrencySymbol) Empty Currency.CurrencyError Currency.OneShotCurrency)
    let cs = Currency.currencySymbol cur
        v  = Value.singleton cs usdt amount
    forM_ wallets $ \w -> do
        let pkh = pubKeyHash $ walletPubKey w
        when (pkh /= ownPK) $ do
            tx <- submitTx $ mustPayToPubKey pkh v
            awaitTxConfirmed $ txId tx
    tell $ Last $ Just cs
  where
    amount :: Integer
    amount = 100_000_000

wallets :: [Wallet]
wallets = [knownWallet i | i <- [1 .. 5]]
