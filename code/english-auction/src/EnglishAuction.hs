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
import           Data.Text                        (Text)
import           GHC.Generics                     (Generic)
import           Plutus.Contract                  hiding (when)
import qualified PlutusTx                         as PlutusTx
import           PlutusTx.Prelude                 hiding (unless)
import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import qualified Ledger.Scripts                   as Scripts
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger.Value                     as Value
import           Playground.Contract              (ensureKnownCurrencies, printSchemas, stage, printJson)
import           Playground.TH                    (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types                 (KnownCurrency (..))
import           Schema                           (ToSchema)
import           Text.Printf                      (printf)

data Auction = Auction
    { aSeller   :: !PubKeyHash
    , aDeadline :: !Slot
    , aMinBid   :: !Integer
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''Auction
PlutusTx.makeLift ''Auction

data Bid = MkBid !PubKeyHash !Integer
    deriving Show

PlutusTx.unstableMakeIsData ''Bid
PlutusTx.makeLift ''Bid

data AuctionAction = Start Auction | Bid Bid | Success | Failure
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

type AuctionSchema =
    BlockchainActions
        .\/ Endpoint "start" StartParams

start :: HasBlockchainActions s => StartParams -> Contract w s Text ()
start StartParams{..} = do
    pkh <- pubKeyHash <$> ownPubKey
    let a = Auction
                { aSeller = pkh
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

endpoints :: Contract () AuctionSchema Text ()
endpoints = start' >> endpoints
  where
    start' = endpoint @"start" >>= start

mkSchemaDefinitions ''AuctionSchema

myToken :: KnownCurrency
myToken = KnownCurrency (ValidatorHash "ffff") "Token" (TokenName "T" :| [])

mkKnownCurrencies ['myToken]
