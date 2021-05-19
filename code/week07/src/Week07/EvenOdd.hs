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

module Week07.EvenOdd
    ( Game (..)
    , GameChoice (..)
    , FirstParams (..)
    , SecondParams (..)
    , GameSchema
    , endpoints
    ) where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (FromJSON, ToJSON)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Plutus.Contract      as Contract hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Ledger.Value
import           Playground.Contract  (ToSchema)
import           Prelude              (Semigroup (..))
import qualified Prelude

data Game = Game
    { gFirst          :: !PubKeyHash
    , gSecond         :: !PubKeyHash
    , gStake          :: !Integer
    , gPlayDeadline   :: !Slot
    , gRevealDeadline :: !Slot
    , gToken          :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Game

data GameChoice = Zero | One
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    Zero == Zero = True
    One  == One  = True
    _    == _    = False

PlutusTx.unstableMakeIsData ''GameChoice

data GameDatum = GameDatum ByteString (Maybe GameChoice)
    deriving Show

instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')

PlutusTx.unstableMakeIsData ''GameDatum

data GameRedeemer = Play GameChoice | Reveal ByteString | ClaimFirst | ClaimSecond
    deriving Show

PlutusTx.unstableMakeIsData ''GameRedeemer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE gameDatum #-}
gameDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe GameDatum
gameDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromData d

{-# INLINABLE mkGameValidator #-}
mkGameValidator :: Game -> ByteString -> ByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsZero' bsOne' dat red ctx =
    traceIfFalse "token missing from input" (assetClassValueOf (txOutValue ownInput) (gToken game) == 1) &&
    case (dat, red) of
        (GameDatum bs Nothing, Play c) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (gSecond game))                                   &&
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            traceIfFalse "second player's stake missing" (lovelaces (txOutValue ownOutput) == (2 * gStake game))            &&
            traceIfFalse "wrong output datum"            (outputDatum == GameDatum bs (Just c))                             &&
            traceIfFalse "missed deadline"               (to (gPlayDeadline game) `contains` txInfoValidRange info)         &&
            traceIfFalse "token missing from output"     (assetClassValueOf (txOutValue ownOutput) (gToken game) == 1)

        (GameDatum bs (Just c), Reveal nonce) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (gFirst game))                                    &&
            traceIfFalse "commit mismatch"               (checkNonce bs nonce c)                                            &&
            traceIfFalse "missed deadline"               (to (gRevealDeadline game) `contains` txInfoValidRange info)       &&
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            traceIfFalse "NFT must go to first player"   nftToFirst

        (GameDatum _ Nothing, ClaimFirst) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (gFirst game))                                    &&
            traceIfFalse "too early"                     (from (1 + gPlayDeadline game) `contains` txInfoValidRange info)   &&
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            traceIfFalse "NFT must go to first player"   nftToFirst

        (GameDatum _ (Just _), ClaimSecond) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (gSecond game))                                   &&
            traceIfFalse "too early"                     (from (1 + gRevealDeadline game) `contains` txInfoValidRange info) &&
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            traceIfFalse "NFT must go to first player"   nftToFirst

        _                              -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "game input missing"
        Just i  -> txInInfoResolved i

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one game output"

    outputDatum :: GameDatum
    outputDatum = case gameDatum ownOutput (`findDatum` info) of
        Nothing -> traceError "game output datum not found"
        Just d  -> d

    checkNonce :: ByteString -> ByteString -> GameChoice -> Bool
    checkNonce bs nonce cSecond = sha2_256 (nonce `concatenate` cFirst) == bs
      where
        cFirst :: ByteString
        cFirst = case cSecond of
            Zero -> bsZero'
            One  -> bsOne'

    nftToFirst :: Bool
    nftToFirst = assetClassValueOf (valuePaidTo info $ gFirst game) (gToken game) == 1

data Gaming
instance Scripts.ScriptType Gaming where
    type instance DatumType Gaming = GameDatum
    type instance RedeemerType Gaming = GameRedeemer

bsZero, bsOne :: ByteString
bsZero = "0"
bsOne  = "1"

gameInst :: Game -> Scripts.ScriptInstance Gaming
gameInst game = Scripts.validator @Gaming
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game
        `PlutusTx.applyCode` PlutusTx.liftCode bsZero
        `PlutusTx.applyCode` PlutusTx.liftCode bsOne)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer

gameValidator :: Game -> Validator
gameValidator = Scripts.validatorScript . gameInst

gameAddress :: Game -> Ledger.Address
gameAddress = scriptAddress . gameValidator

findGameOutput :: HasBlockchainActions s => Game -> Contract w s Text (Maybe (TxOutRef, TxOutTx, GameDatum))
findGameOutput game = do
    utxos <- utxoAt $ gameAddress game
    return $ do
        (oref, o) <- find f $ Map.toList utxos
        dat       <- gameDatum (txOutTxOut o) (`Map.lookup` txData (txOutTxTx o))
        return (oref, o, dat)
  where
    f :: (TxOutRef, TxOutTx) -> Bool
    f (_, o) = assetClassValueOf (txOutValue $ txOutTxOut o) (gToken game) == 1

data FirstParams = FirstParams
    { fpSecond         :: !PubKeyHash
    , fpStake          :: !Integer
    , fpPlayDeadline   :: !Slot
    , fpRevealDeadline :: !Slot
    , fpNonce          :: !ByteString
    , fpCurrency       :: !CurrencySymbol
    , fpTokenName      :: !TokenName
    , fpChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

firstGame :: forall w s. HasBlockchainActions s => FirstParams -> Contract w s Text ()
firstGame fp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let game = Game
            { gFirst          = pkh
            , gSecond         = fpSecond fp
            , gStake          = fpStake fp
            , gPlayDeadline   = fpPlayDeadline fp
            , gRevealDeadline = fpRevealDeadline fp
            , gToken          = AssetClass (fpCurrency fp, fpTokenName fp)
            }
        v    = lovelaceValueOf (fpStake fp) <> assetClassValue (gToken game) 1
        c    = fpChoice fp
        bs   = sha2_256 $ fpNonce fp `concatenate` if c == Zero then bsZero else bsOne
        tx   = Constraints.mustPayToTheScript (GameDatum bs Nothing) v
    ledgerTx <- submitTxConstraints (gameInst game) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "made first move: " ++ show (fpChoice fp)

    void $ awaitSlot $ 1 + fpPlayDeadline fp

    m <- findGameOutput game
    case m of
        Nothing             -> throwError "game output not found"
        Just (oref, o, dat) -> case dat of
            GameDatum _ Nothing -> do
                logInfo @String "second player did not play"
                let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (gameValidator game)
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData ClaimFirst)
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ txId ledgerTx'
                logInfo @String "reclaimed stake"

            GameDatum _ (Just c') | c' == c -> do
                logInfo @String "second player played and lost"
                let lookups = Constraints.unspentOutputs (Map.singleton oref o)                                         <>
                              Constraints.otherScript (gameValidator game)
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData $ Reveal $ fpNonce fp) <>
                              Constraints.mustValidateIn (to $ fpRevealDeadline fp)
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ txId ledgerTx'
                logInfo @String "victory"

            _ -> logInfo @String "second player played and won"

data SecondParams = SecondParams
    { spFirst          :: !PubKeyHash
    , spStake          :: !Integer
    , spPlayDeadline   :: !Slot
    , spRevealDeadline :: !Slot
    , spCurrency       :: !CurrencySymbol
    , spTokenName      :: !TokenName
    , spChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

secondGame :: forall w s. HasBlockchainActions s => SecondParams -> Contract w s Text ()
secondGame sp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let game = Game
            { gFirst          = spFirst sp
            , gSecond         = pkh
            , gStake          = spStake sp
            , gPlayDeadline   = spPlayDeadline sp
            , gRevealDeadline = spRevealDeadline sp
            , gToken          = AssetClass (spCurrency sp, spTokenName sp)
            }
    m <- findGameOutput game
    case m of
        Just (oref, o, GameDatum bs Nothing) -> do
            logInfo @String "running game found"
            let token   = assetClassValue (gToken game) 1
            let v       = let x = lovelaceValueOf (spStake sp) in x <> x <> token
                c       = spChoice sp
                lookups = Constraints.unspentOutputs (Map.singleton oref o)                            <>
                          Constraints.otherScript (gameValidator game)                                 <>
                          Constraints.scriptInstanceLookups (gameInst game)
                tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData $ Play c) <>
                          Constraints.mustPayToTheScript (GameDatum bs $ Just c) v                     <>
                          Constraints.mustValidateIn (to $ spPlayDeadline sp)
            ledgerTx <- submitTxConstraintsWith @Gaming lookups tx
            let tid = txId ledgerTx
            void $ awaitTxConfirmed tid
            logInfo @String $ "made second move: " ++ show (spChoice sp)

            void $ awaitSlot $ 1 + spRevealDeadline sp

            m' <- findGameOutput game
            case m' of
                Nothing             -> logInfo @String "first player won"
                Just (oref', o', _) -> do
                    logInfo @String "first player didn't reveal"
                    let lookups' = Constraints.unspentOutputs (Map.singleton oref' o')                              <>
                                   Constraints.otherScript (gameValidator game)
                        tx'      = Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toData ClaimSecond) <>
                                   Constraints.mustValidateIn (from $ 1 + spRevealDeadline sp)                      <>
                                   Constraints.mustPayToPubKey (spFirst sp) token
                    ledgerTx' <- submitTxConstraintsWith @Gaming lookups' tx'
                    void $ awaitTxConfirmed $ txId ledgerTx'
                    logInfo @String "second player won"

        _ -> logInfo @String "no running game found"

type GameSchema = BlockchainActions .\/ Endpoint "first" FirstParams .\/ Endpoint "second" SecondParams

endpoints :: Contract () GameSchema Text ()
endpoints = (first `select` second) >> endpoints
  where
    first  = endpoint @"first"  >>= firstGame
    second = endpoint @"second" >>= secondGame
