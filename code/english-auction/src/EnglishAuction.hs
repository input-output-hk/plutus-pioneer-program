{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module EnglishAuction
    ( Auction (..)
    , StartParams (..)
    , AuctionSchema
    , start
    , endpoints
    , schemas
    , ensureKnownCurrencies
    , printJson
    , printSchemas
    , registeredKnownCurrencies
    , stage
    ) where

import           Control.Monad                    hiding (fmap)
import           Data.Aeson                       (ToJSON, FromJSON)
import           Data.List.NonEmpty               (NonEmpty (..))
import           Data.Map                         as Map
import           Data.Text                        (pack, Text)
import           GHC.Generics                     (Generic)
import           Plutus.Contract                  hiding (when)
import qualified PlutusTx                         as PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup(..), unless)
import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import qualified Ledger.Scripts                   as Scripts
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger.Value                     as Value
import           Ledger.Ada                       as Ada
import           Playground.Contract              (ensureKnownCurrencies, printSchemas, stage, printJson)
import           Playground.TH                    (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types                 (KnownCurrency (..))
import           Prelude                          (Semigroup (..))
import           Schema                           (ToSchema)
import           Text.Printf                      (printf)

data Auction = Auction
    { aSeller   :: !PubKeyHash
    , aDeadline :: !Slot
    , aMinBid   :: !Integer
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''Auction
PlutusTx.makeLift ''Auction

data Bid = Bid
    { bBidder :: !PubKeyHash
    , bBid    :: !Integer
    } deriving Show

PlutusTx.unstableMakeIsData ''Bid
PlutusTx.makeLift ''Bid

data AuctionAction = Start Auction | MkBid Bid | Close
    deriving Show

PlutusTx.unstableMakeIsData ''AuctionAction
PlutusTx.makeLift ''AuctionAction

data AuctionDatum = AuctionDatum
    { adAuction    :: !Auction
    , adHighestBid :: !(Maybe Bid)
    } deriving Show

PlutusTx.unstableMakeIsData ''AuctionDatum
PlutusTx.makeLift ''AuctionDatum

data Auctioning
instance Scripts.ScriptType Auctioning where
    type instance RedeemerType Auctioning = AuctionAction
    type instance DatumType Auctioning = AuctionDatum

{-# INLINABLE minBid #-}
minBid :: AuctionDatum -> Integer
minBid AuctionDatum{..} = case adHighestBid of
    Nothing      -> aMinBid adAuction
    Just Bid{..} -> bBid + 1

{-# INLINABLE mkAuctionValidator #-}
mkAuctionValidator :: AuctionDatum -> AuctionAction -> ValidatorCtx -> Bool
mkAuctionValidator _ _ _ = True

auctionInstance :: Scripts.ScriptInstance Auctioning
auctionInstance = Scripts.validator @Auctioning
    $$(PlutusTx.compile [|| mkAuctionValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @AuctionDatum @AuctionAction

auctionValidator :: Validator
auctionValidator = Scripts.validatorScript auctionInstance

auctionHash :: Ledger.ValidatorHash
auctionHash = Scripts.validatorHash auctionValidator

auctionAddress :: Ledger.Address
auctionAddress = ScriptAddress auctionHash

data StartParams = StartParams
    { spDeadline :: !Slot
    , spMinBid   :: !Integer
    , spCurrency :: !CurrencySymbol
    , spToken    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data BidParams = BidParams
    { bpCurrency :: !CurrencySymbol
    , bpToken    :: !TokenName
    , bpBid      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type AuctionSchema =
    BlockchainActions
        .\/ Endpoint "start" StartParams
        .\/ Endpoint "bid"   BidParams

start :: (HasBlockchainActions s, AsContractError e) => StartParams -> Contract w s e ()
start StartParams{..} = do
    pkh <- pubKeyHash <$> ownPubKey
    let a = Auction
                { aSeller   = pkh
                , aDeadline = spDeadline
                , aMinBid   = spMinBid
                }
        d = AuctionDatum
                { adAuction    = a
                , adHighestBid = Nothing
                }
        v = Value.singleton spCurrency spToken 1
        tx = mustPayToTheScript d v
    ledgerTx <- submitTxConstraints auctionInstance tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "started auction %s for token %s" (show a) (show v)

bid :: forall w s. HasBlockchainActions s => BidParams -> Contract w s Text ()
bid BidParams{..} = do
    (oref, o, d@AuctionDatum{..}) <- findAuction
    logInfo @String $ printf "found auction utxo with datum %s" (show d)
    when (bpBid < minBid d) $
        throwError $ pack $ printf "bid lower than minimal bid %d" (minBid d)
    pkh <- pubKeyHash <$> ownPubKey
    let b  = Bid {bBidder = pkh, bBid = bpBid}
        d' = d {adHighestBid = Just b}
        v  = Value.singleton bpCurrency bpToken 1 <> Ada.lovelaceValueOf bpBid
        r  = Redeemer $ PlutusTx.toData $ MkBid b

        lookups = Constraints.scriptInstanceLookups auctionInstance <>
                  Constraints.otherScript auctionValidator          <>
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx      = case adHighestBid of
                    Nothing      -> mustPayToTheScript d' v                            <>
                                    mustValidateIn (to $ aDeadline adAuction)          <>
                                    mustSpendScriptOutput oref r
                    Just Bid{..} -> mustPayToTheScript d' v                            <>
                                    mustPayToPubKey bBidder (Ada.lovelaceValueOf bBid) <>
                                    mustValidateIn (to $ aDeadline adAuction)          <>
                                    mustSpendScriptOutput oref r
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String ""
  where
    findAuction :: Contract w s Text (TxOutRef, TxOutTx, AuctionDatum)
    findAuction = do
        utxos <- utxoAt $ ScriptAddress auctionHash
        let xs = [ (oref, o)
                 | (oref, o) <- Map.toList utxos
                 , Value.valueOf (txOutValue $ txOutTxOut o) bpCurrency bpToken == 1
                 ]
        case xs of
            [(oref, o)] -> case txOutType $ txOutTxOut o of
                PayToPubKey   -> throwError "unexpected out type"
                PayToScript h -> case Map.lookup h $ txData $ txOutTxTx o of
                    Nothing        -> throwError "datum not found"
                    Just (Datum e) -> case PlutusTx.fromData e of
                        Nothing -> throwError "datum has wrong type"
                        Just d  -> return (oref, o, d)
            _           -> throwError "auction utxo not found"

endpoints :: Contract () AuctionSchema Text ()
endpoints = (start' `select` bid') >> endpoints
  where
    start' = endpoint @"start" >>= start
    bid'   = endpoint @"bid"   >>= bid

mkSchemaDefinitions ''AuctionSchema

myToken :: KnownCurrency
myToken = KnownCurrency (ValidatorHash "f") "Token" (TokenName "T" :| [])

mkKnownCurrencies ['myToken]
