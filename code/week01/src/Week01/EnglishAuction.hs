{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE RecordWildCards #-}

module Week01.EnglishAuction
    ( Auction (..)
    , StartParams (..), BidParams (..), CloseParams (..)
    , AuctionSchema
    , start, bid, close
    , endpoints
    , schemas
    , ensureKnownCurrencies
    , printJson
    , printSchemas
    , registeredKnownCurrencies
    , stage
    , test
    ) where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import           Data.Default               (Default (..))
import           Data.Text            (pack, Text)
import           GHC.Generics         (Generic)
import           Plutus.Contract      hiding (when)
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import qualified PlutusTx.Prelude     as Plutus
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import           Ledger.Ada           as Ada
import           Playground.Contract  (ensureKnownCurrencies, printSchemas, stage, printJson)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (Semigroup (..))
import           Schema               (ToSchema)
import           Text.Printf          (printf)
import           Plutus.Trace.Emulator      as Emulator
import           Wallet.Emulator.Wallet

data Auction = Auction
    { aSeller   :: !PubKeyHash
    , aDeadline :: !Slot
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
instance Scripts.ScriptType Auctioning where
    type instance RedeemerType Auctioning = AuctionAction
    type instance DatumType Auctioning = AuctionDatum

{-# INLINABLE minBid #-}
minBid :: AuctionDatum -> Integer
minBid AuctionDatum{..} = case adHighestBid of
    Nothing      -> aMinBid adAuction
    Just Bid{..} -> bBid + 1


{-# INLINABLE auctionDatum #-}
auctionDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe AuctionDatum
auctionDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromData d

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

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "auction input missing"
        Just i  -> txInInfoResolved i

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one auction output"

    outputDatum :: AuctionDatum
    outputDatum = case auctionDatum ownOutput (`findDatum` info) of
        Nothing -> traceError "auction output datum not found"
        Just d  -> d

    auction :: Auction
    auction = adAuction ad

    tokenValue :: Value
    tokenValue = Value.singleton (aCurrency auction) (aToken auction) 1

    correctInputValue :: Bool
    correctInputValue = txOutValue ownInput == case adHighestBid ad of
        Nothing      -> tokenValue
        Just Bid{..} -> tokenValue Plutus.<> Ada.lovelaceValueOf bBid

    sufficientBid :: Integer -> Bool
    sufficientBid amount = amount >= minBid ad

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
            os = [ o | o <- txInfoOutputs info, txOutAddress o == pubKeyHashAddress bBidder                 ]
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
        [o] = [ o' | o' <- txInfoOutputs info, txOutValue o' == v ]
      in
        txOutAddress o == pubKeyHashAddress h

auctionInstance :: Scripts.ScriptInstance Auctioning
auctionInstance = Scripts.validator @Auctioning
    $$(PlutusTx.compile [|| mkAuctionValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @AuctionDatum @AuctionAction

auctionValidator :: Validator
auctionValidator = Scripts.validatorScript auctionInstance

auctionAddress :: Ledger.Address
auctionAddress = scriptAddress auctionValidator

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

data CloseParams = CloseParams
    { cpCurrency :: !CurrencySymbol
    , cpToken    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type AuctionSchema =
    BlockchainActions
        .\/ Endpoint "start" StartParams
        .\/ Endpoint "bid"   BidParams
        .\/ Endpoint "close" CloseParams

start :: (HasBlockchainActions s, AsContractError e) => StartParams -> Contract w s e ()
start StartParams{..} = do
    pkh <- pubKeyHash <$> Plutus.Contract.ownPubKey
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
    logInfo @String $ printf "about to submit tx"
    ledgerTx <- submitTxConstraints auctionInstance tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "started auction %s for token %s" (show a) (show v)

bid :: forall w s. HasBlockchainActions s => BidParams -> Contract w s Text ()
bid BidParams{..} = do
    auction <- findAuction bpCurrency bpToken
    case auction of 
        Just (oref, o, d@AuctionDatum{..}) -> do
            logInfo @String $ printf "found auction utxo with datum %s" (show d)

            when (bpBid < minBid d) $
                Plutus.Contract.throwError $ pack $ printf "bid lower than minimal bid %d" (minBid d)
            pkh <- pubKeyHash <$> Plutus.Contract.ownPubKey
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
            logInfo @String $ printf "made bid of %d lovelace in auction %s for token (%s, %s)"
                bpBid
                (show adAuction)
                (show bpCurrency)
                (show bpToken)
        Nothing -> Plutus.Contract.throwError "auction not found!"

close :: forall w s. HasBlockchainActions s => CloseParams -> Contract w s Text ()
close CloseParams{..} = do
    auction <- findAuction cpCurrency cpToken
    case auction of 
            Just (oref, o, d@AuctionDatum{..}) -> do
                logInfo @String $ printf "found auction utxo with datum %s" (show d)

                let t      = Value.singleton cpCurrency cpToken 1
                    r      = Redeemer $ PlutusTx.toData Close
                    seller = aSeller adAuction

                    lookups = Constraints.scriptInstanceLookups auctionInstance <>
                            Constraints.otherScript auctionValidator          <>
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
            Nothing -> Plutus.Contract.throwError "auction not found"

findAuction :: HasBlockchainActions s => CurrencySymbol -> TokenName -> Contract w s Text (Maybe (TxOutRef, TxOutTx, AuctionDatum))
findAuction cs tn = do
    utxos <- utxoAt auctionAddress 
    return $ do
        (oref, o) <- find f $ Map.toList utxos
        dat       <- auctionDatum (txOutTxOut o) (`Map.lookup` txData (txOutTxTx o))
        return (oref, o, dat)
  where
    f :: (TxOutRef, TxOutTx) -> Bool
    f (_, o) = assetClassValueOf (txOutValue $ txOutTxOut o) (AssetClass (cs, tn)) == 1

endpoints :: Contract () AuctionSchema Text ()
endpoints = (start' `select` bid' `select` close') >> endpoints
  where
    start' = endpoint @"start" >>= start
    bid'   = endpoint @"bid"   >>= bid
    close' = endpoint @"close" >>= close

mkSchemaDefinitions ''AuctionSchema

myToken :: KnownCurrency
myToken = KnownCurrency (ValidatorHash "f") "Token" (TokenName "T" :| [])

mkKnownCurrencies ['myToken]

test :: IO ()
test = runEmulatorTraceIO' def emCfg myTrace
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig $ Left $ Map.fromList
        [ (Wallet 1, v <> assetClassValue (AssetClass (currencySymbol "T", TokenName "Token")) 1)
        , (Wallet 2, v)
        , (Wallet 3, v)
        ]

    v :: Value
    v = Ada.lovelaceValueOf 1000_000_000

myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    h3 <- activateContractWallet (Wallet 3) endpoints
    callEndpoint @"start" h1 StartParams {
        spDeadline = 10
        , spMinBid = 1_000_000
        , spCurrency = currencySymbol "T"
        , spToken    = TokenName "Token"
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"bid" h2 BidParams {  
         bpCurrency = currencySymbol "T"
        , bpToken   = TokenName "Token"
        , bpBid      = 500_000
    }
    void $ Emulator.waitNSlots 1
    callEndpoint @"bid" h3 BidParams {  
         bpCurrency = currencySymbol "T"
        , bpToken   = TokenName "Token"
        , bpBid      = 1_000_000
    }
    void $ Emulator.waitUntilSlot 10
    callEndpoint @"close" h1 CloseParams {  
        cpCurrency = currencySymbol "T"
        , cpToken   = TokenName "Token"
    }
