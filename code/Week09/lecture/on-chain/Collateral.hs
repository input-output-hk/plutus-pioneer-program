{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Collateral 
  ( apiCollateralScript
  , collateralScriptAsShortBs
  , CollateralDatum (..)
  , CollateralLock (..)
  , stablecoinTokenName
  ) where

import           Cardano.Api.Shelley            (PlutusScript (..), PlutusScriptV2)
import           Codec.Serialise                (serialise)
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import           Plutus.V2.Ledger.Api
import           Plutus.V1.Ledger.Value
import qualified Plutus.V2.Ledger.Contexts      as V2
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude
import           Utilities            (wrapValidator)

tus :: BuiltinString
tus = "TUS"

stablecoinTokenName :: TokenName 
stablecoinTokenName = TokenName $ encodeUtf8 tus


-- Change to inline datum so people will be able to liquidate.

data CollateralLock = Unlocked | Locked
    deriving Prelude.Show

instance Eq CollateralLock where
  (==) Unlocked Unlocked = True
  (==) Locked Unlocked   = False
  (==) Unlocked Locked   = False
  (==) Locked Locked     = True

data CollateralDatum = CollateralDatum
    { colMintingPolicyId  :: CurrencySymbol 
    , colOwner            :: PubKeyHash
    , colStablecoinAmount :: Integer
    , colLock             :: CollateralLock
    } deriving Prelude.Show

PlutusTx.unstableMakeIsData ''CollateralLock
PlutusTx.unstableMakeIsData ''CollateralDatum

data CollateralRedeemer = Lock | Redeem | Liquidate

PlutusTx.unstableMakeIsData ''CollateralRedeemer

{-# INLINABLE signedByCollateralOwner #-}
signedByCollateralOwner :: TxInfo -> CollateralDatum -> Bool
signedByCollateralOwner info dat = V2.txSignedBy info $ colOwner dat

{-# INLINABLE mkValidator #-}
mkValidator :: CollateralDatum -> CollateralRedeemer -> ScriptContext -> Bool
mkValidator dat Lock ctx =
    traceIfFalse "collateral owner's signature missing" (signedByCollateralOwner info dat) &&
    traceIfFalse "initial stablecoin amount must be 0" checkInitialAmount &&
    traceIfFalse "collateral must be unlocked" checkCollateralLock &&
    traceIfFalse "minted amount must be positive" checkMintPositive &&
    traceIfFalse "invalid new output's datum" checkOutputDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkInitialAmount :: Bool
    checkInitialAmount = colStablecoinAmount dat == 0

    checkCollateralLock :: Bool
    checkCollateralLock = colLock dat == Unlocked

    mintedAmount :: Integer
    mintedAmount = assetClassValueOf (txInfoMint info) (AssetClass (colMintingPolicyId dat, stablecoinTokenName))

    checkMintPositive :: Bool
    checkMintPositive = mintedAmount > 0

    ownOutput :: TxOut
    ownOutput = case V2.getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one collateral output"
    
    outputDatum :: Maybe CollateralDatum
    outputDatum = case txOutDatum ownOutput of
                    NoOutputDatum -> Nothing
                    OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d
                    OutputDatumHash dh -> do 
                                        Datum d <- V2.findDatum dh info
                                        PlutusTx.fromBuiltinData d

    checkOutputDatum :: Bool
    checkOutputDatum = case outputDatum of
        Nothing     -> False
        Just newDat -> colMintingPolicyId newDat == colMintingPolicyId dat &&
                       colOwner newDat == colOwner dat &&
                       colStablecoinAmount newDat == mintedAmount &&
                       colLock newDat == Locked

mkValidator dat Redeem ctx =
    traceIfFalse "collateral owner's signature missing" (signedByCollateralOwner info dat) &&
    case colLock dat of
        Unlocked -> True
        Locked   -> traceIfFalse "minted stablecoin amount mismatch" checkStablecoinAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    forgeValue :: Value
    forgeValue = txInfoMint info

    checkStablecoinAmount :: Bool
    checkStablecoinAmount = negate (colStablecoinAmount dat) == assetClassValueOf forgeValue (AssetClass (colMintingPolicyId dat, stablecoinTokenName))

mkValidator dat Liquidate ctx =
    traceIfFalse "minted stablecoin amount mismatch" checkStablecoinAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    forgeValue :: Value
    forgeValue = txInfoMint info

    checkStablecoinAmount :: Bool
    checkStablecoinAmount = negate (colStablecoinAmount dat) == assetClassValueOf forgeValue (AssetClass (colMintingPolicyId dat, stablecoinTokenName))


{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator mkValidator

validator :: Validator
validator = mkValidatorScript 
    $$(PlutusTx.compile [|| mkWrappedValidator ||])

script :: Script
script = unValidatorScript validator

collateralScriptAsShortBs :: SBS.ShortByteString
collateralScriptAsShortBs = SBS.toShort . LBS.toStrict $ serialise script

apiCollateralScript :: PlutusScript PlutusScriptV2
apiCollateralScript = PlutusScriptSerialised collateralScriptAsShortBs
