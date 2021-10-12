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

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week01.EnglishAuction where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import           Data.Text            (pack, Text)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import qualified PlutusTx             as PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import qualified PlutusTx.Prelude     as Plutus
import           Plutus.ChainIndex.Tx (ChainIndexTx (..))
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Scripts       as Scripts
import qualified Ledger.Typed.Scripts as Scripts hiding (validatorHash)
import           Ledger.Value         as Value
import           Ledger.Ada           as Ada
import           Ledger.Tx            (ChainIndexTxOut (..))
import           Playground.Contract  (ensureKnownCurrencies, printSchemas, stage, printJson)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Schema               (ToSchema)
import           Text.Printf          (printf)

data Auction = Auction
    { aSeller   :: !PubKeyHash
    , aDeadline :: !POSIXTime
    , aMinBid   :: !Integer
    , aCurrency :: !CurrencySymbol
    , aToken    :: !TokenName
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq Auction where
    {-# INLINABLE (==) #-}
    a == b = (aSeller   a == aSeller   b) &&
             (aDeadline a == aDeadline b) &&
             (aMinBid   a == aMinBid   b) &&
             (aCurrency a == aCurrency b) &&
             (aToken    a == aToken    b)

PlutusTx.unstableMakeIsData ''Auction
PlutusTx.makeLift ''Auction

data Bid = Bid
    { bBidder :: !PubKeyHash
    , bBid    :: !Integer
    } deriving Show

instance Eq Bid where
    {-# INLINABLE (==) #-}
    b == c = (bBidder b == bBidder c) &&
             (bBid    b == bBid    c)

PlutusTx.unstableMakeIsData ''Bid
PlutusTx.makeLift ''Bid

data AuctionAction = MkBid Bid | Close
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
instance Scripts.ValidatorTypes Auctioning where
    type instance RedeemerType Auctioning = AuctionAction
    type instance DatumType Auctioning = AuctionDatum

{-# INLINABLE minBid #-}
minBid :: AuctionDatum -> Integer
minBid AuctionDatum{..} = case adHighestBid of
    Nothing      -> aMinBid adAuction
    Just Bid{..} -> bBid + 1

{-# INLINABLE mkAuctionValidator #-}
mkAuctionValidator :: AuctionDatum -> AuctionAction -> ScriptContext -> Bool
mkAuctionValidator ad redeemer ctx =
    traceIfFalse "wrong input value" correctInputValue &&
    case redeemer of
        MkBid b@Bid{..} ->
            traceIfFalse "bid too low" (sufficientBid bBid)                &&
            traceIfFalse "wrong output datum" (correctBidOutputDatum b)    &&
            traceIfFalse "wrong output value" (correctBidOutputValue bBid) &&
            traceIfFalse "wrong refund"       correctBidRefund             &&
            traceIfFalse "too late"           correctBidSlotRange
        Close           ->
            traceIfFalse "too early" correctCloseSlotRange &&
            case adHighestBid ad of
                Nothing      ->
                    traceIfFalse "expected seller to get token" (getsValue (aSeller auction) tokenValue)
                Just Bid{..} ->
                    traceIfFalse "expected highest bidder to get token" (getsValue bBidder tokenValue) &&
                    traceIfFalse "expected seller to get highest bid" (getsValue (aSeller auction) $ Ada.lovelaceValueOf bBid)

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    input :: TxInInfo
    input =
      let
        isScriptInput i = case (txOutDatumHash . txInInfoResolved) i of
            Nothing -> False
            Just _  -> True
        xs = [i | i <- txInfoInputs info, isScriptInput i]
      in
        case xs of
            [i] -> i
            _   -> traceError "expected exactly one script input"

    inVal :: Value
    inVal = txOutValue . txInInfoResolved $ input

    auction :: Auction
    auction = adAuction ad

    tokenValue :: Value
    tokenValue = Value.singleton (aCurrency auction) (aToken auction) 1

    correctInputValue :: Bool
    correctInputValue = inVal == case adHighestBid ad of
        Nothing      -> tokenValue
        Just Bid{..} -> tokenValue Plutus.<> Ada.lovelaceValueOf bBid

    sufficientBid :: Integer -> Bool
    sufficientBid amount = amount >= minBid ad

    ownOutput   :: TxOut
    outputDatum :: AuctionDatum
    (ownOutput, outputDatum) = case getContinuingOutputs ctx of
        [o] -> case txOutDatumHash o of
            Nothing   -> traceError "wrong output type"
            Just h -> case findDatum h info of
                Nothing        -> traceError "datum not found"
                Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
                    Just ad' -> (o, ad')
                    Nothing  -> traceError "error decoding data"
        _   -> traceError "expected exactly one continuing output"

    correctBidOutputDatum :: Bid -> Bool
    correctBidOutputDatum b = (adAuction outputDatum == auction)   &&
                              (adHighestBid outputDatum == Just b)

    correctBidOutputValue :: Integer -> Bool
    correctBidOutputValue amount =
        txOutValue ownOutput == tokenValue Plutus.<> Ada.lovelaceValueOf amount

    correctBidRefund :: Bool
    correctBidRefund = case adHighestBid ad of
        Nothing      -> True
        Just Bid{..} ->
          let
            os = [ o
                 | o <- txInfoOutputs info
                 , txOutAddress o == pubKeyHashAddress bBidder
                 ]
          in
            case os of
                [o] -> txOutValue o == Ada.lovelaceValueOf bBid
                _   -> traceError "expected exactly one refund output"

    correctBidSlotRange :: Bool
    correctBidSlotRange = to (aDeadline auction) `contains` txInfoValidRange info

    correctCloseSlotRange :: Bool
    correctCloseSlotRange = from (aDeadline auction) `contains` txInfoValidRange info

    getsValue :: PubKeyHash -> Value -> Bool
    getsValue h v =
      let
        [o] = [ o'
              | o' <- txInfoOutputs info
              , txOutValue o' == v
              ]
      in
        txOutAddress o == pubKeyHashAddress h

auctionTypedValidator :: Scripts.TypedValidator Auctioning
auctionTypedValidator = Scripts.mkTypedValidator @Auctioning
    $$(PlutusTx.compile [|| mkAuctionValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator

auctionValidator :: Validator
auctionValidator = Scripts.validatorScript auctionTypedValidator

auctionAddress :: Ledger.ValidatorHash
auctionAddress = Scripts.validatorHash auctionValidator

data StartParams = StartParams
    { spDeadline :: !POSIXTime
    , spMinBid   :: !Integer
    , spCurrency :: !CurrencySymbol
    , spToken    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data BidParams = BidParams
    { bpCurrency :: !CurrencySymbol
    , bpToken    :: !TokenName
    , bpBid      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data CloseParams = CloseParams
    { cpCurrency :: !CurrencySymbol
    , cpToken    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type AuctionSchema =
            Endpoint "start" StartParams
        .\/ Endpoint "bid"   BidParams
        .\/ Endpoint "close" CloseParams

start :: AsContractError e => StartParams -> Contract w s e ()
start StartParams{..} = do
    pkh <- pubKeyHash <$> ownPubKey
    let a = Auction
                { aSeller   = pkh
                , aDeadline = spDeadline
                , aMinBid   = spMinBid
                , aCurrency = spCurrency
                , aToken    = spToken
                }
        d = AuctionDatum
                { adAuction    = a
                , adHighestBid = Nothing
                }
        v = Value.singleton spCurrency spToken 1
        tx = mustPayToTheScript d v
    ledgerTx <- submitTxConstraints auctionTypedValidator tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "started auction %s for token %s" (show a) (show v)

bid :: forall w s. BidParams -> Contract w s Text ()
bid BidParams{..} = do
    (oref, o, d@AuctionDatum{..}) <- findAuction bpCurrency bpToken
    logInfo @String $ printf "found auction utxo with datum %s" (show d)

    when (bpBid < minBid d) $
        throwError $ pack $ printf "bid lower than minimal bid %d" (minBid d)
    pkh <- pubKeyHash <$> ownPubKey
    let b  = Bid {bBidder = pkh, bBid = bpBid}
        d' = d {adHighestBid = Just b}
        v  = Value.singleton bpCurrency bpToken 1 <> Ada.lovelaceValueOf bpBid
        r  = Redeemer $ PlutusTx.toBuiltinData $ MkBid b

        lookups = Constraints.typedValidatorLookups auctionTypedValidator <>
                  Constraints.otherScript auctionValidator                <>
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
    logInfo @String $ printf "made bid of %d lovelace in auction %s for token (%s, %s)"
        bpBid
        (show adAuction)
        (show bpCurrency)
        (show bpToken)

close :: forall w s. CloseParams -> Contract w s Text ()
close CloseParams{..} = do
    (oref, o, d@AuctionDatum{..}) <- findAuction cpCurrency cpToken
    logInfo @String $ printf "found auction utxo with datum %s" (show d)

    let t      = Value.singleton cpCurrency cpToken 1
        r      = Redeemer $ PlutusTx.toBuiltinData Close
        seller = aSeller adAuction

        lookups = Constraints.typedValidatorLookups auctionTypedValidator <>
                  Constraints.otherScript auctionValidator                <>
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx      = case adHighestBid of
                    Nothing      -> mustPayToPubKey seller t                          <>
                                    mustValidateIn (from $ aDeadline adAuction)       <>
                                    mustSpendScriptOutput oref r
                    Just Bid{..} -> mustPayToPubKey bBidder t                         <>
                                    mustPayToPubKey seller (Ada.lovelaceValueOf bBid) <>
                                    mustValidateIn (from $ aDeadline adAuction)       <>
                                    mustSpendScriptOutput oref r
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "closed auction %s for token (%s, %s)"
        (show adAuction)
        (show cpCurrency)
        (show cpToken)

findAuction :: CurrencySymbol -> TokenName -> Contract w s Text (TxOutRef, ChainIndexTxOut, AuctionDatum) 
findAuction cs tn = do
    utxos <- utxosTxOutTxAt $ scriptAddress auctionValidator
    let xs = [ (oref, (utxo, tx))
             | (oref, (utxo, tx)) <- Map.toList utxos
             , Value.valueOf (_ciTxOutValue utxo) cs tn == 1
             ]
    case xs of
        [(oref, (utxo, tx))] -> case txOutDatumHash $ toTxOut utxo of
            Nothing   -> throwError "unexpected out type"
            Just h -> case Map.lookup h $ _citxData tx of 
                Nothing        -> throwError "datum not found"
                Just (Datum e) -> case PlutusTx.fromBuiltinData e of
                    Nothing -> throwError "datum has wrong type"
                    Just d@AuctionDatum{..}
                        | aCurrency adAuction == cs && aToken adAuction == tn -> return (oref, utxo, d)
                        | otherwise                                           -> throwError "auction token missmatch"
        _           -> throwError "auction utxo not found"

endpoints :: Contract () AuctionSchema Text ()
endpoints = awaitPromise
           $ start' `select` bid' `select` close'
 where
   start' = endpoint @"start"  start
   bid'   = endpoint @"bid"    bid
   close' = endpoint @"close"  close

mkSchemaDefinitions ''AuctionSchema

myToken :: KnownCurrency
myToken = KnownCurrency (ValidatorHash "f") "Token" (TokenName "T" :| [])

mkKnownCurrencies ['myToken]
