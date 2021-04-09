{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week02.FortyTwo
    ( give
    , grab
    , FortyTwoSchema
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

{-# INLINABLE mkFortyTwoValidator #-}
mkFortyTwoValidator :: Data -> Data -> Data -> ()
mkFortyTwoValidator _ (I n) _
    | n == 42                 = ()
mkFortyTwoValidator _ _     _ = traceError "UNEXPECTED REDEEMER!"

fortyTwoValidator :: Validator
fortyTwoValidator = mkValidatorScript $$(PlutusTx.compile [|| mkFortyTwoValidator ||])

fortyTwoHash :: Ledger.ValidatorHash
fortyTwoHash = Scripts.validatorHash fortyTwoValidator

fortyTwoAddress :: Ledger.Address
fortyTwoAddress = ScriptAddress fortyTwoHash

type FortyTwoSchema =
    BlockchainActions
        .\/ Endpoint "give" Integer
        .\/ Endpoint "grab" Integer

give :: (HasBlockchainActions s, AsContractError e) => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToOtherScript fortyTwoHash (Datum $ Constr 0 []) $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => Integer -> Contract w s e ()
grab r = do
    utxos <- utxoAt $ ScriptAddress fortyTwoHash
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript fortyTwoValidator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ I r | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () FortyTwoSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>= grab

mkSchemaDefinitions ''FortyTwoSchema

mkKnownCurrencies []
