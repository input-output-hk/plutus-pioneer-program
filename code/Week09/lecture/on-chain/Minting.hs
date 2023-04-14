{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}


module Minting
    ( MintParams (..)
    , apiMintScript
    ) where

import Cardano.Api.Shelley                      (PlutusScript (..), PlutusScriptV2)
import           Codec.Serialise                (serialise)
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import           Plutus.V2.Ledger.Api
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Address
import           Plutus.V2.Ledger.Contexts      as V2
import qualified PlutusTx
import           PlutusTx.Prelude
import           Prelude                        (Show)
import           Oracle              (oracleValue)
import           Collateral          (CollateralDatum (..), CollateralLock (..), stablecoinTokenName)
import           Utilities            (wrapPolicy)


-- TODO: Update to use oracle as reference input
-- Only to ideantify the collateral

collateralMinPercent :: Integer
collateralMinPercent = 150

data MintParams = MintParams
    { mpOracleValidator     :: ValidatorHash
    , mpCollateralValidator :: ValidatorHash
    } deriving Show

PlutusTx.makeLift ''MintParams

data MintRedeemer = Mint | Redeem | Liquidate

PlutusTx.unstableMakeIsData ''MintRedeemer

{-# INLINABLE getColDatum #-}
getColDatum :: OutputDatum -> (DatumHash -> Maybe Datum) -> Maybe CollateralDatum
getColDatum od f = case od of
            NoOutputDatum         -> Nothing
            OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d
            OutputDatumHash dh    -> do 
                                    Datum d <- f dh
                                    PlutusTx.fromBuiltinData d

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
-- lovelaces = getLovelace . fromValue
lovelaces v =  valueOf v adaSymbol adaToken

-- Oracle has ada price in cents so $1 is 100 in oracle's data so rate needs to be divided by 100
-- collateralAmount is in lovelaces so final calculation needs to be divided by 1_000_000
-- (collateralAmount `divide` collateralMinPercent * 100) * rate `divide` 100 `divide` 1_000_000
{-# INLINABLE calcMaxMint #-}
calcMaxMint :: Integer -> Integer -> Integer
calcMaxMint collateralAmount rate = (collateralAmount `divide` collateralMinPercent * rate) `divide` 1_000_000

{-# INLINABLE mkPolicy #-}
mkPolicy :: MintParams -> MintRedeemer -> ScriptContext -> Bool
mkPolicy mp Mint ctx =
    traceIfFalse "oracle input missing" checkOracleInput &&
    traceIfFalse "minted amount must be positive" checkMintPositive &&
    traceIfFalse "minted amount exceeds max" checkMaxMint &&
    traceIfFalse "invalid datum at collateral output" checkDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    oracleInputs :: [TxOut]
    oracleInputs = [ o
                    | i <- txInfoInputs info
                    , let o = txInInfoResolved i
                    , txOutAddress o == scriptHashAddress (mpOracleValidator mp)
                    ] ++
                    [ o
                    | i <- txInfoReferenceInputs info
                    , let o = txInInfoResolved i
                    , txOutAddress o == scriptHashAddress (mpOracleValidator mp)
                    ]

    checkOracleInput :: Bool
    checkOracleInput = length oracleInputs == 1

    mintedAmount :: Integer
    mintedAmount = assetClassValueOf (txInfoMint info) (AssetClass (ownCurrencySymbol ctx, stablecoinTokenName))

    checkMintPositive :: Bool
    checkMintPositive = mintedAmount > 0

    -- TODO: Why can't we have more than one output?
    collateralOutput :: (OutputDatum, Value)
    collateralOutput = case scriptOutputsAt (mpCollateralValidator mp) info of
                        [(h, v)] -> (h, v)
                        _        -> traceError "expected exactly one collateral output"

    (collateralOutDat, collateralValue) = collateralOutput

    collateralAmount :: Integer
    collateralAmount = lovelaces collateralValue

    oracleInput :: TxOut
    oracleInput = case oracleInputs of
                    [o] -> o
                    _   -> traceError "expected exactly one oracle input"

    oracleValue' :: Integer
    oracleValue' = case oracleValue oracleInput (`findDatum` info) of
        Nothing -> traceError "oracle value not found"
        Just x  -> x

    checkMaxMint :: Bool
    checkMaxMint = calcMaxMint collateralAmount oracleValue' >= mintedAmount

    collateralDatum :: Maybe CollateralDatum
    collateralDatum = getColDatum collateralOutDat (`V2.findDatum` info)

    checkDatum :: Bool
    checkDatum = case collateralDatum of
        Nothing -> False
        Just d  -> colMintingPolicyId d  == ownCurrencySymbol ctx &&
                   colStablecoinAmount d == mintedAmount &&
                   colLock d             == Locked &&
                   V2.txSignedBy info (colOwner d)

mkPolicy mp Redeem ctx =
    traceIfFalse "redeem amount mismatch or owner's signature missing" checkRedeem
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    redeemedAmount :: Integer
    redeemedAmount = assetClassValueOf (txInfoMint info) (AssetClass (ownCurrencySymbol ctx, stablecoinTokenName))

    collateralOutput :: (OutputDatum, Value) -- TODO not output, could be input
    collateralOutput = case scriptOutputsAt (mpCollateralValidator mp) info of
                        [(h, v)] -> (h, v)
                        _        -> traceError "expected exactly one collateral output"

    (collateralOutDat, _) = collateralOutput

    collateralDatum :: Maybe CollateralDatum
    collateralDatum = getColDatum collateralOutDat (`V2.findDatum` info)

    checkRedeem :: Bool
    checkRedeem = case collateralDatum of
        Nothing -> False
        Just d  -> negate (colStablecoinAmount d) == redeemedAmount &&
                   V2.txSignedBy info (colOwner d)

mkPolicy mp Liquidate ctx =
    traceIfFalse "oracle input missing" checkOracleInput && -- This is not needed. We already check for it while checking for liquidation
    traceIfFalse "invalid redeem amount" checkRedeem &&
    traceIfFalse "liquidation threshold not reached" checkLiquidation
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    oracleInputs :: [TxOut]
    oracleInputs = [ o
                    | i <- txInfoInputs info
                    , let o = txInInfoResolved i
                    , txOutAddress o == scriptHashAddress (mpOracleValidator mp)
                    ] ++
                    [ o
                    | i <- txInfoReferenceInputs info
                    , let o = txInInfoResolved i
                    , txOutAddress o == scriptHashAddress (mpOracleValidator mp)
                    ]

    checkOracleInput :: Bool
    checkOracleInput = length oracleInputs == 1

    redeemedAmount :: Integer
    redeemedAmount = assetClassValueOf (txInfoMint info) (AssetClass (ownCurrencySymbol ctx, stablecoinTokenName))

    collateralOutput :: (OutputDatum, Value)
    collateralOutput = case scriptOutputsAt (mpCollateralValidator mp) info of
                        [(h, v)] -> (h, v)
                        _        -> traceError "expected exactly one collateral output"

    (collateralOutDat, collateralValue) = collateralOutput

    collateralDatum :: Maybe CollateralDatum
    collateralDatum = getColDatum collateralOutDat (`V2.findDatum` info)

    checkRedeem :: Bool
    checkRedeem = case collateralDatum of
        Nothing -> False
        Just d  -> negate (colStablecoinAmount d) == redeemedAmount

    collateralAmount :: Integer
    collateralAmount = lovelaces collateralValue

    oracleInput :: TxOut
    oracleInput = case oracleInputs of
                    [o] -> o
                    _   -> traceError "expected exactly one oracle input"

    oracleValue' :: Integer
    oracleValue' = case oracleValue oracleInput (`findDatum` info) of
        Nothing -> traceError "oracle value not found"
        Just x  -> x

    checkLiquidation :: Bool
    checkLiquidation = calcMaxMint collateralAmount oracleValue' < negate redeemedAmount

{-# INLINABLE  mkWrappedPolicy #-}
mkWrappedPolicy :: MintParams -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicy = wrapPolicy . mkPolicy

policy :: MintParams -> MintingPolicy
policy np = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkWrappedPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode np

script :: MintParams -> Script
script = unMintingPolicyScript . policy

validator :: MintParams -> Validator
validator = Validator . script

scriptAsCbor :: MintParams -> LBS.ByteString
scriptAsCbor = serialise . validator

apiMintScript :: MintParams -> PlutusScript PlutusScriptV2
apiMintScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . scriptAsCbor
