{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week03.Homework2 where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract      hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (Semigroup (..))
import           Text.Printf          (printf)

{-# INLINABLE mkValidator #-}
mkValidator :: PubKeyHash -> Slot -> () -> ScriptContext -> Bool
mkValidator hsh rng () ctx -- FIX ME!
    | checkSig hsh && checkDeadline1 rng = True
    | checkSig hsh && checkDeadline2 rng = True
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    
    checkSig :: PubKeyHash -> Bool
    checkSig hsh = hsh `elem` txInfoSignatories info

    checkDeadline1 :: Slot -> Bool
    checkDeadline1 rng = to rng `contains` txInfoValidRange info
    
    checkDeadline2 :: Slot -> Bool
    checkDeadline2 rng = from rng `contains` txInfoValidRange info

data Vesting
instance Scripts.ScriptType Vesting where
    type instance DatumType Vesting = Slot
    type instance RedeemerType Vesting = ()

inst :: PubKeyHash -> Scripts.ScriptInstance Vesting
inst hsh = Scripts.validator @Vesting -- IMPLEMENT ME!
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode hsh)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Slot @()

validator :: PubKeyHash -> Validator
validator =  Scripts.validatorScript . inst -- IMPLEMENT ME!

scrAddress :: PubKeyHash -> Ledger.Address
scrAddress = scriptAddress . validator -- IMPLEMENT ME!

data GiveParams = GiveParams
    { gpBeneficiary :: !PubKeyHash
    , gpDeadline    :: !Slot
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
    BlockchainActions
        .\/ Endpoint "give" GiveParams
        .\/ Endpoint "grab" ()

give :: (HasBlockchainActions s, AsContractError e) => GiveParams -> Contract w s e ()
give gp = do
    let p  = gpBeneficiary gp
        d  = gpDeadline gp
        tx = mustPayToTheScript d $ Ada.lovelaceValueOf $ gpAmount gp
    ledgerTx <- submitTxConstraints (inst p) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => Contract w s e ()
grab = do
    now   <- currentSlot
    pkh   <- pubKeyHash <$> ownPubKey
    utxos <- Map.filter (isSuitable now) <$> utxoAt (scrAddress pkh)
    if Map.null utxos
        then logInfo @String $ "no gifts available"
        else do
            let orefs   = fst <$> Map.toList utxos
                lookups = Constraints.unspentOutputs utxos        <>
                          Constraints.otherScript (validator pkh)
                tx :: TxConstraints Void Void
                tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | oref <- orefs] <>
                          mustValidateIn (from now)
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "collected gifts"
  where
    isSuitable :: Slot -> TxOutTx -> Bool
    isSuitable now o = case txOutDatumHash $ txOutTxOut o of
        Nothing -> False
        Just h  -> case Map.lookup h $ txData $ txOutTxTx o of
            Nothing        -> False
            Just (Datum e) -> case PlutusTx.fromData e of
                Nothing -> False
                Just d  -> d <= now

endpoints :: Contract () VestingSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>  grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []
