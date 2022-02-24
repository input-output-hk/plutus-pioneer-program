{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Week06.Token.OffChain
    ( TokenParams (..)
    , adjustAndSubmit, adjustAndSubmitWith
    , mintToken
    ) where

import           Control.Monad               hiding (fmap)
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromJust)
import           Data.OpenApi.Schema         (ToSchema)
import           Data.Text                   (Text, pack)
import           Data.Void                   (Void)
import           GHC.Generics                (Generic)
import           Plutus.Contract             as Contract
import           Plutus.Contract.Wallet      (getUnspentOutput)
import qualified PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Ledger                      hiding (mint, singleton)
import           Ledger.Constraints          as Constraints
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value
import           Prelude                     (Semigroup (..), Show (..), String)
import qualified Prelude
import           Text.Printf                 (printf)

import           Week06.Token.OnChain
import           Week06.Utils                (getCredentials)

data TokenParams = TokenParams
    { tpToken   :: !TokenName
    , tpAmount  :: !Integer
    , tpAddress :: !Address
    } deriving (Prelude.Eq, Prelude.Ord, Generic, FromJSON, ToJSON, ToSchema, Show)

adjustAndSubmitWith :: ( PlutusTx.FromData (Scripts.DatumType a)
                       , PlutusTx.ToData (Scripts.RedeemerType a)
                       , PlutusTx.ToData (Scripts.DatumType a)
                       , AsContractError e
                       )
                    => ScriptLookups a
                    -> TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a)
                    -> Contract w s e CardanoTx
adjustAndSubmitWith lookups constraints = do
    unbalanced <- adjustUnbalancedTx <$> mkTxConstraints lookups constraints
    Contract.logDebug @String $ printf "unbalanced: %s" $ show unbalanced
    unsigned <- balanceTx unbalanced
    Contract.logDebug @String $ printf "balanced: %s" $ show unsigned
    signed <- submitBalancedTx unsigned
    Contract.logDebug @String $ printf "signed: %s" $ show signed
    return signed

adjustAndSubmit :: ( PlutusTx.FromData (Scripts.DatumType a)
                   , PlutusTx.ToData (Scripts.RedeemerType a)
                   , PlutusTx.ToData (Scripts.DatumType a)
                   , AsContractError e
                   )
                => Scripts.TypedValidator a
                -> TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a)
                -> Contract w s e CardanoTx
adjustAndSubmit inst = adjustAndSubmitWith $ Constraints.typedValidatorLookups inst

mintToken :: TokenParams -> Contract w s Text CurrencySymbol
mintToken tp = do
    Contract.logDebug @String $ printf "started minting: %s" $ show tp
    let addr = tpAddress tp
    case getCredentials addr of
        Nothing      -> Contract.throwError $ pack $ printf "expected pubkey address, but got %s" $ show addr
        Just (x, my) -> do
            oref <- getUnspentOutput
            o    <- fromJust <$> Contract.txOutFromRef oref
            Contract.logDebug @String $ printf "picked UTxO at %s with value %s" (show oref) (show $ _ciTxOutValue o)

            let tn          = tpToken tp
                amt         = tpAmount tp
                cs          = tokenCurSymbol oref tn amt
                val         = Value.singleton cs tn amt
                c           = case my of
                    Nothing -> Constraints.mustPayToPubKey x val
                    Just y  -> Constraints.mustPayToPubKeyAddress x y val
                lookups     = Constraints.mintingPolicy (tokenPolicy oref tn amt) <>
                              Constraints.unspentOutputs (Map.singleton oref o)
                constraints = Constraints.mustMintValue val          <>
                              Constraints.mustSpendPubKeyOutput oref <>
                              c

            void $ adjustAndSubmitWith @Void lookups constraints
            Contract.logInfo @String $ printf "minted %s" (show val)
            return cs
