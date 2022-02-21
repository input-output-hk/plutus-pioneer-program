{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
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

import           Control.Monad               hiding (fmap)
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.Map                    as Map
import           Data.OpenApi.Schema         (ToSchema)
import           Data.Text                   (Text, pack)
import           Data.Void                   (Void)
import           GHC.Generics                (Generic)
import           Plutus.Contract             as Contract
import           Plutus.V1.Ledger.Ada        (lovelaceValueOf, toValue)
import           Plutus.V1.Ledger.Credential
import qualified PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Ledger                      hiding (mint, singleton)
import           Ledger.Constraints          as Constraints
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value
import           Prelude                     (Semigroup (..), Show (..), String)
import qualified Prelude                     as Prelude
import           Text.Printf                 (printf)

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

getUTxO :: Address -> Contract w s Text (TxOutRef, ChainIndexTxOut)
getUTxO addr = do
    Contract.logDebug @String $ printf "started getting UTxO at address %s" $ show addr
    utxos <- utxosAt addr
    case Map.toList utxos of
        []            -> Contract.throwError $ pack $ printf "no UTxO found at address %s" $ show addr
        (oref, o) : _ -> do
            Contract.logDebug @String $ printf "found UTxO at %s with value %s" (show oref) (show $ _ciTxOutValue o)
            return (oref, o)

mintToken :: TokenParams -> Contract w s Text CurrencySymbol
mintToken tp = do
    Contract.logDebug @String $ printf "started minting: %s" $ show tp
    let addr = tpAddress tp
    (oref, o) <- getUTxO addr
    let tn      = tpToken tp
        amt     = tpAmount tp
        cs      = tokenCurSymbol oref tn amt
        val     = Value.singleton cs tn amt
        val'    = lovelaceValueOf 5_000_000 --val <> toValue minAdaTxOut
    c <- case getCredentials addr of
        Nothing           -> Contract.throwError $ pack $ printf "expected pubkey address, but got %s" $ show addr
        Just (x, Nothing) -> return $ Constraints.mustPayToPubKey x val'
        Just (x, Just y)  -> return $ Constraints.mustPayToPubKeyAddress x y val'
    let lookups     = Constraints.mintingPolicy (tokenPolicy oref tn amt) <> Constraints.unspentOutputs (Map.singleton oref o)
        constraints = Constraints.mustMintValue val          <>
                      Constraints.mustSpendPubKeyOutput oref <>
                      c
    unbalanced <- mkTxConstraints @Void lookups constraints
    Contract.logDebug @String $ printf "unbalanced: %s" $ show unbalanced

    unsigned   <- balanceTx unbalanced
    Contract.logDebug @String $ printf "balanced: %s" $ show unsigned

    signed     <- submitBalancedTx unsigned
    Contract.logDebug @String $ printf "signed: %s" $ show signed

    Contract.logInfo @String $ printf "minted %s" (show val)
    return cs

getCredentials :: Address -> Maybe (PaymentPubKeyHash, Maybe StakePubKeyHash)
getCredentials (Address x y) = case x of
    ScriptCredential _   -> Nothing
    PubKeyCredential pkh ->
      let
        ppkh = PaymentPubKeyHash pkh
      in
        case y of
            Nothing                 -> Just (ppkh, Nothing)
            Just (StakingPtr _ _ _) -> Nothing
            Just (StakingHash h)    -> case h of
                ScriptCredential _    -> Nothing
                PubKeyCredential pkh' -> Just (ppkh, Just $ StakePubKeyHash pkh')
