{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week02.Burn
    ( burn
    , grab
    , BurnSchema
    , endpoints
    , schemas
    , registeredKnownCurrencies
    , printJson
    , printSchemas
    , ensureKnownCurrencies
    , stage
    ) where

import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Plutus.Contract     hiding (when)
import           PlutusTx            (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude    hiding (Semigroup(..), unless)
import           Ledger              hiding (singleton)
import           Ledger.Constraints  as Constraints
import qualified Ledger.Scripts      as Scripts
import           Ledger.Ada          as Ada
import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))
import           Prelude             (Semigroup (..))
import           Text.Printf         (printf)

{-# INLINABLE mkBurnValidator #-}
mkBurnValidator :: Data -> Data -> Data -> ()
mkBurnValidator _ _ _ = traceError "NO WAY!"

burnValidator :: Validator
burnValidator = mkValidatorScript $$(PlutusTx.compile [|| mkBurnValidator ||])

burnHash :: Ledger.ValidatorHash
burnHash = Scripts.validatorHash burnValidator

burnAddress :: Ledger.Address
burnAddress = ScriptAddress burnHash

type BurnSchema =
    BlockchainActions
        .\/ Endpoint "burn" Integer
        .\/ Endpoint "grab" ()

burn :: (HasBlockchainActions s, AsContractError e) => Integer -> Contract w s e ()
burn amount = do
    let tx = mustPayToOtherScript burnHash (Datum $ Constr 0 []) $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "burnt %d lovelace" amount

grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => Contract w s e ()
grab = do
    utxos <- utxoAt $ ScriptAddress burnHash
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript burnValidator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ I 17 | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () BurnSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"burn" >>= burn
    grab' = endpoint @"grab" >>  grab

mkSchemaDefinitions ''BurnSchema

mkKnownCurrencies []
