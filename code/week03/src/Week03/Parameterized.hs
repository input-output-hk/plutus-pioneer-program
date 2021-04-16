{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week03.Parameterized
    ( give
    , grab
    , ParameterizedSchema
    , endpoints
    , schemas
    , registeredKnownCurrencies
    , printJson
    , printSchemas
    , ensureKnownCurrencies
    , stage
    ) where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract      hiding (when)
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (Semigroup (..))
import           Schema               (ToSchema)
import           Text.Printf          (printf)

{-# INLINABLE mkParameterizedValidator #-}
mkParameterizedValidator :: Integer -> () -> Integer -> ScriptContext -> Bool
mkParameterizedValidator r () n _ = traceIfFalse "UNEXPECTED REDEEMER" (n == r)

data Parameterizing
instance Scripts.ScriptType Parameterizing where
    type instance RedeemerType Parameterizing = Integer
    type instance DatumType Parameterizing = ()

parameterizedInstance :: Integer -> Scripts.ScriptInstance Parameterizing
parameterizedInstance r = Scripts.validator @Parameterizing
    ($$(PlutusTx.compile [|| mkParameterizedValidator ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode r)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @Integer

parameterizedValidator :: Integer -> Validator
parameterizedValidator = Scripts.validatorScript . parameterizedInstance

parameterizedAddress :: Integer -> Ledger.Address
parameterizedAddress = scriptAddress . parameterizedValidator

type ParameterizedSchema =
    BlockchainActions
        .\/ Endpoint "give" GiveParams
        .\/ Endpoint "grab" GrabParams

data GiveParams = GiveParams
    { giveAmount    :: Integer
    , giveParameter :: Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data GrabParams = GrabParams
    { grabParameter :: Integer
    , grabRedeemer  :: Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

give :: (HasBlockchainActions s, AsContractError e) => GiveParams -> Contract w s e ()
give p = do
    let amount = giveAmount p
    let tx     = mustPayToTheScript () $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTxConstraints (parameterizedInstance $ giveParameter p) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => GrabParams -> Contract w s e ()
grab p = do
    let par = grabParameter p
    utxos <- utxoAt $ parameterizedAddress par
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos <>
                  Constraints.otherScript (parameterizedValidator par)
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ I $ grabRedeemer p | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () ParameterizedSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>= grab

mkSchemaDefinitions ''ParameterizedSchema

mkKnownCurrencies []
