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

module Week05.Solution1 where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract hiding (when)
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (Semigroup (..))
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> Slot -> ScriptContext -> Bool
mkPolicy pkh deadline ctx =
    traceIfFalse "signature missing" (txSignedBy info pkh) &&
    traceIfFalse "deadline missed"   (to deadline `contains` txInfoValidRange info)
  where
    info = scriptContextTxInfo ctx

policy :: PubKeyHash -> Slot -> Scripts.MonetaryPolicy
policy pkh deadline = mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| \pkh' deadline' -> Scripts.wrapMonetaryPolicy $ mkPolicy pkh' deadline' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh
    `PlutusTx.applyCode`
    PlutusTx.liftCode deadline

curSymbol :: PubKeyHash -> Slot -> CurrencySymbol
curSymbol pkh deadline = scriptCurrencySymbol $ policy pkh deadline

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpDeadline  :: !Slot
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type SignedSchema =
    BlockchainActions
        .\/ Endpoint "mint" MintParams

mint :: MintParams -> Contract w SignedSchema Text ()
mint mp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    now <- Contract.currentSlot
    let deadline = mpDeadline mp
    if now > deadline
        then Contract.logError @String "deadline passed"
        else do
            let val     = Value.singleton (curSymbol pkh deadline) (mpTokenName mp) (mpAmount mp)
                lookups = Constraints.monetaryPolicy $ policy pkh deadline
                tx      = Constraints.mustForgeValue val <> Constraints.mustValidateIn (to deadline)
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () SignedSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''SignedSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn       = "ABC"
        deadline = 10
    h <- activateContractWallet (Wallet 1) endpoints
    callEndpoint @"mint" h $ MintParams
        { mpTokenName = tn
        , mpDeadline  = deadline
        , mpAmount    = 555
        }
    void $ Emulator.waitNSlots 15
    callEndpoint @"mint" h $ MintParams
        { mpTokenName = tn
        , mpDeadline  = deadline
        , mpAmount    = 555
        }
    void $ Emulator.waitNSlots 1
