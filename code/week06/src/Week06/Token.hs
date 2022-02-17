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

module Week06.Token
    ( tokenPolicy
    , tokenCurSymbol
    , TokenParams (..)
    , mintToken
    ) where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Map               as Map
import           Data.OpenApi.Schema    (ToSchema)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (Semigroup (..), Show (..), String)
import qualified Prelude                as Prelude
import           Text.Printf            (printf)

{-# INLINABLE mkTokenPolicy #-}
mkTokenPolicy :: TxOutRef -> TokenName -> Integer -> () -> ScriptContext -> Bool
mkTokenPolicy oref tn amt () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                              traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt')] -> tn' == tn && amt' == amt
        _                -> False

tokenPolicy :: TxOutRef -> TokenName -> Integer -> Scripts.MintingPolicy
tokenPolicy oref tn amt = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' amt' -> Scripts.wrapMintingPolicy $ mkTokenPolicy oref' tn' amt' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt

tokenCurSymbol :: TxOutRef -> TokenName -> Integer -> CurrencySymbol
tokenCurSymbol oref tn = scriptCurrencySymbol . tokenPolicy oref tn

data TokenParams = TokenParams
    { tpToken   :: !TokenName
    , tpAmount  :: !Integer
    , tpAddress :: !Address
    } deriving (Prelude.Eq, Prelude.Ord, Generic, FromJSON, ToJSON, ToSchema, Show)

mintToken :: TokenParams -> Contract () Empty Text ()
mintToken tp = do
    Contract.logDebug @String $ printf "started minting: %s" $ show tp
    utxos <- utxosAt $ tpAddress tp
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let tn      = tpToken tp
                amt     = tpAmount tp
            let val     = Value.singleton (tokenCurSymbol oref tn amt) tn amt
                lookups = Constraints.mintingPolicy (tokenPolicy oref tn amt) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            Contract.logDebug @String $ printf "picked utxo for minting: %s" $ show oref
            void $ submitTxConstraintsWith @Void lookups tx
            Contract.logInfo @String $ printf "minted %s" (show val)
