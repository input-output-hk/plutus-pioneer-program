{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Collateral where

import Plutus.V2.Ledger.Api     ( mkValidatorScript,
                                   Validator,
                                   Datum(Datum),
                                   OutputDatum(OutputDatumHash, NoOutputDatum, OutputDatum),
                                   TxOut(txOutDatum),
                                   ScriptContext(scriptContextTxInfo),
                                   TxInfo(txInfoMint),
                                   BuiltinData,
                                   PubKeyHash,
                                   CurrencySymbol,
                                   TokenName(TokenName) )
import Plutus.V1.Ledger.Value    ( assetClassValueOf, AssetClass(AssetClass))
import Plutus.V2.Ledger.Contexts ( findDatum, getContinuingOutputs, txSignedBy )
import PlutusTx                  ( compile, unstableMakeIsData, FromData(fromBuiltinData) )
import PlutusTx.Prelude          ( Bool(..),
                                   Integer,
                                   Maybe(..),
                                   negate,
                                   traceError,
                                   (&&),
                                   traceIfFalse,
                                   encodeUtf8,
                                   ($),
                                   Ord((>)),
                                   Eq(..) )
import           Utilities        (wrapValidator, writeValidatorToFile)
import qualified Prelude


---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: HELPER FUNCTIONS/TYPES ------------------------------------

stablecoinTokenName :: TokenName 
stablecoinTokenName = TokenName $ encodeUtf8 "USDP"

data CollateralLock = Unlocked | Locked
    deriving Prelude.Show

instance Eq CollateralLock where
  (==) Locked   Locked   = True
  (==) Unlocked Unlocked = True
  (==) _      _          = False
unstableMakeIsData ''CollateralLock

---------------------------------------------------------------------------------------------------
------------------------------------ ON-CHAIN: VALIDATOR ------------------------------------------

-- Datum containing all the relevant information 
data CollateralDatum = CollateralDatum
    { colMintingPolicyId  :: CurrencySymbol 
    , colOwner            :: PubKeyHash
    , colStablecoinAmount :: Integer
    , colLock             :: CollateralLock
    } deriving Prelude.Show
unstableMakeIsData ''CollateralDatum

-- We can lock or redeem our own collateral or liquidate someone else's
data CollateralRedeemer = Lock | Redeem | Liquidate
unstableMakeIsData ''CollateralRedeemer


{-# INLINABLE mkValidator #-}
mkValidator :: CollateralDatum -> CollateralRedeemer -> ScriptContext -> Bool
mkValidator dat r ctx = case r of
    Lock      -> traceIfFalse "collateral owner's signature missing" checkSignedByCollOwner &&
                 traceIfFalse "initial stablecoin amount must be 0" checkInitialAmount &&
                 traceIfFalse "collateral must be unlocked" checkCollateralLock &&
                 traceIfFalse "minted amount must be positive" checkMintPositive &&
                 traceIfFalse "invalid new output's datum" checkOutputDatum
    Redeem    -> traceIfFalse "collateral owner's signature missing" checkSignedByCollOwner &&
                 case colLock dat of
                     Unlocked -> True
                     Locked   -> traceIfFalse "burned stablecoin amount mismatch" checkStablecoinAmount
    Liquidate -> traceIfFalse "burned stablecoin amount mismatch" checkStablecoinAmount

  where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Check if the transaction is signed by the collateral owner
    checkSignedByCollOwner :: Bool
    checkSignedByCollOwner = txSignedBy info $ colOwner dat

    -- Check if the initial stablecoin amount is 0
    checkInitialAmount :: Bool
    checkInitialAmount = colStablecoinAmount dat == 0

    -- Check if the collateral is unlocked
    checkCollateralLock :: Bool
    checkCollateralLock = colLock dat == Unlocked

    -- Amount of stablecoins minted in this transaction
    mintedAmount :: Integer
    mintedAmount = assetClassValueOf (txInfoMint info) (AssetClass (colMintingPolicyId dat, stablecoinTokenName))

    -- Check if the minted amount is positive
    checkMintPositive :: Bool
    checkMintPositive = mintedAmount > 0

    -- Get the collateral script's output
    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one collateral output"
    
    -- Get the new datum from the collateral script's output
    outputDatum :: Maybe CollateralDatum
    outputDatum = case txOutDatum ownOutput of
                    NoOutputDatum -> Nothing
                    OutputDatum (Datum d) -> fromBuiltinData d
                    OutputDatumHash dh -> do 
                                        Datum d <- findDatum dh info
                                        fromBuiltinData d

    -- Check if the new output's datum has the correct values
    checkOutputDatum :: Bool
    checkOutputDatum = case outputDatum of
        Nothing     -> False
        Just newDat -> colMintingPolicyId newDat == colMintingPolicyId dat &&
                       colOwner newDat == colOwner dat &&
                       colStablecoinAmount newDat == mintedAmount &&
                       colLock newDat == Locked

    -- Check that the amount of stablecoins burned matches the amont at the collateral's datum
    checkStablecoinAmount :: Bool
    checkStablecoinAmount = negate (colStablecoinAmount dat) == mintedAmount

---------------------------------------------------------------------------------------------------
------------------------------ COMPILE AND SERIALIZE VALIDATOR ------------------------------------

{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator mkValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedValidator ||])

saveCollateralScript :: Prelude.IO ()
saveCollateralScript = writeValidatorToFile "assets/collateral.plutus" validator
