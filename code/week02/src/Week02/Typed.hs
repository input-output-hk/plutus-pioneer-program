{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week02.Typed
    ( give
    , grab
    , TypedSchema
    , endpoints
    , schemas
    , registeredKnownCurrencies
    , printJson
    , printSchemas
    , ensureKnownCurrencies
    , stage
    ) where

import           Control.Monad        hiding (fmap)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           Plutus.Contract      hiding (when)
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Scripts       as Scripts
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (Semigroup (..))
import           Text.Printf          (printf)

{-# INLINABLE mkTypedValidator #-}
mkTypedValidator :: () -> Integer -> ValidatorCtx -> Bool
mkTypedValidator () n _ = traceIfFalse "UNEXPECTED REDEEMER" (n == 42)

data Typing
instance Scripts.ScriptType Typing where
    type instance RedeemerType Typing = Integer
    type instance DatumType Typing = ()

typedInstance :: Scripts.ScriptInstance Typing
typedInstance = Scripts.validator @Typing
    $$(PlutusTx.compile [|| mkTypedValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @Integer

typedValidator :: Validator
typedValidator = Scripts.validatorScript typedInstance

typedHash :: Ledger.ValidatorHash
typedHash = Scripts.validatorHash typedValidator

typedAddress :: Ledger.Address
typedAddress = ScriptAddress typedHash

type TypedSchema =
    BlockchainActions
        .\/ Endpoint "give" Integer
        .\/ Endpoint "grab" Integer

give :: (HasBlockchainActions s, AsContractError e) => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTxConstraints typedInstance tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => Integer -> Contract w s e ()
grab r = do
    utxos <- utxoAt $ ScriptAddress typedHash
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript typedValidator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ I r | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () TypedSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>= grab

mkSchemaDefinitions ''TypedSchema

mkKnownCurrencies []
