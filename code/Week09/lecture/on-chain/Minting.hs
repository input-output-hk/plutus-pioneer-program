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
import Plutus.V2.Ledger.Api
    ( Script,
      Validator(Validator),
      Datum(Datum),
      OutputDatum(..),
      TxOut(txOutAddress),
      Value,
      ScriptContext(scriptContextTxInfo),
      TxInfo(txInfoReferenceInputs, txInfoMint),
      BuiltinData,
      unMintingPolicyScript,
      mkMintingPolicyScript,
      adaToken,
      adaSymbol,
      MintingPolicy,
      TxInInfo(txInInfoResolved),
      ValidatorHash )
import Plutus.V1.Ledger.Value
    ( assetClassValueOf,
      AssetClass(AssetClass),
      valueOf )
import Plutus.V1.Ledger.Address ( scriptHashAddress )
import Plutus.V2.Ledger.Contexts
    ( findDatum,
      txSignedBy,
      scriptOutputsAt,
      ownCurrencySymbol )
import PlutusTx
    ( compile,
      unstableMakeIsData,
      FromData(fromBuiltinData),
      liftCode,
      applyCode,
      makeLift )
import PlutusTx.Prelude
    ( Bool(False),
      Integer,
      Maybe(..),
      (.),
      negate,
      traceError,
      (&&),
      traceIfFalse,
      ($),
      Ord((<), (>), (>=)),
      Eq((==)),
      length,
      divide,
      MultiplicativeSemigroup((*)) )
import qualified Prelude                        (Show)
import           Oracle              (parseOracleDatum)
import           Collateral          (CollateralDatum (..), CollateralLock (..), stablecoinTokenName)
import           Utilities            (wrapPolicy)


---------------------------------------------------------------------------------------------------
------------------------------- ON-CHAIN: HELPER FUNCTIONS/TYPES ----------------------------------

-- | collateralMinPercent = 150 means that locked collateral has to be at least 150% the minted amount
collateralMinPercent :: Integer
collateralMinPercent = 150


---------------------------------------------------------------------------------------------------
------------------------------------ ON-CHAIN: VALIDATOR ------------------------------------------

-- Oracle and collateral validator hashes are passed as parameters
data MintParams = MintParams
    { mpOracleValidator     :: ValidatorHash
    , mpCollateralValidator :: ValidatorHash
    } deriving Prelude.Show
makeLift ''MintParams

-- We can mint or burn our own stablecoins and liquidate someone else's.
data MintRedeemer = Mint | Burn | Liquidate
unstableMakeIsData ''MintRedeemer


{-# INLINABLE mkPolicy #-}
mkPolicy :: MintParams -> MintRedeemer -> ScriptContext -> Bool
mkPolicy mp r ctx = case r of
    Mint      -> traceIfFalse "oracle input missing" checkOracleInput &&
                 traceIfFalse "minted amount must be positive" checkMintPositive &&
                 traceIfFalse "minted amount exceeds max" checkMaxMint &&
                 traceIfFalse "invalid datum at collateral output" checkDatum

    Burn      -> traceIfFalse "invalid burn amount" checkBurnAmountMatchesColDatum &&
                 traceIfFalse "owner's signature missing" checkColOwner

    Liquidate -> traceIfFalse "invalid burn amount" checkBurnAmountMatchesColDatum &&
                 traceIfFalse "liquidation threshold not reached" checkLiquidation
                 -- We check the oracle's input implicitly using `getOracleInput` while checking for liquidation
                 
    where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    --------- ORACLE-RELATED FUNCTIONS ------------

    -- Get list with only oracle inputs
    oracleInputs :: [TxOut]
    oracleInputs = [ o
                   | i <- txInfoReferenceInputs info
                   , let o = txInInfoResolved i
                   , txOutAddress o == scriptHashAddress (mpOracleValidator mp)
                   ]

    -- Check if there is exactly one oracle input
    checkOracleInput :: Bool
    checkOracleInput = length oracleInputs == 1

    -- Get the oracle's input
    getOracleInput :: TxOut
    getOracleInput = case oracleInputs of
                    [o] -> o
                    _   -> traceError "expected exactly one oracle input"

    -- Get the oracle's value
    oracleValue :: Integer
    oracleValue = case parseOracleDatum getOracleInput (`findDatum` info) of
        Nothing -> traceError "oracle value not found"
        Just x  -> x


    --------- MINTING-RELATED FUNCTIONS ------------

    -- Get amount of stablecoins to minted (or burned if negative) in this transaction
    mintedAmount :: Integer
    mintedAmount = assetClassValueOf (txInfoMint info) (AssetClass (ownCurrencySymbol ctx, stablecoinTokenName))

    -- Check that the amount of stablecoins minted is positive
    checkMintPositive :: Bool
    checkMintPositive = mintedAmount > 0

    {-
    maxMint calculates the maximum amount of stablecoins that can be minted with the given collateral.

    Oracle has ada price in cents so $1 is 100 in oracle's data. So rate needs to be divided by 100.
    Also, collateralAmount is in lovelaces, so final calculation needs to be divided by 1_000_000.
    Without reductions: (collateralAmount `divide` collateralMinPercent * 100) * rate `divide` 100 `divide` 1_000_000
    With reductions:
    -}
    maxMint :: Integer
    maxMint = (collateralAmount `divide` collateralMinPercent * oracleValue) `divide` 1_000_000

    -- Check that the amount of stablecoins minted does not exceed the maximum
    checkMaxMint :: Bool
    checkMaxMint = maxMint >= mintedAmount

    --------- COLLATERAL-RELATED FUNCTIONS ------------

    -- Get the collateral's output datum and value
    collateralOutput :: (OutputDatum, Value)
    collateralOutput = case scriptOutputsAt (mpCollateralValidator mp) info of
                        [(h, v)] -> (h, v)
                        _        -> traceError "expected exactly one collateral output"

    -- Name the collateral's output datum and value
    (collateralOutDat, collateralValue) = collateralOutput

    -- Get the collateral's amount as an integer
    collateralAmount :: Integer
    collateralAmount = valueOf collateralValue adaSymbol adaToken

    -- | Helper function to extract the value from the collateral datum
    collateralDatum :: Maybe CollateralDatum
    collateralDatum = case collateralOutDat of
                NoOutputDatum         -> Nothing
                OutputDatum (Datum d) -> fromBuiltinData d
                OutputDatumHash dh    -> do 
                                        Datum d <- findDatum dh info
                                        fromBuiltinData d

    -- Check that the collateral's datum has the correct values
    checkDatum :: Bool
    checkDatum = case collateralDatum of
        Nothing -> False
        Just d  -> colMintingPolicyId d  == ownCurrencySymbol ctx &&
                   colStablecoinAmount d == mintedAmount &&
                   colLock d             == Locked &&
                   txSignedBy info (colOwner d)

    -- Check that the amount of stablecoins burned matches the amont at the collateral's datum
    checkBurnAmountMatchesColDatum :: Bool
    checkBurnAmountMatchesColDatum = case collateralDatum of
        Nothing -> False
        Just d  -> negate (colStablecoinAmount d) == mintedAmount

    -- Check that the owner's signature is present
    checkColOwner :: Bool
    checkColOwner = case collateralDatum of
        Nothing -> False
        Just d  -> txSignedBy info (colOwner d)

    -- Check that the collateral's value is low enough to liquidate
    checkLiquidation :: Bool
    checkLiquidation = maxMint < negate mintedAmount


---------------------------------------------------------------------------------------------------
------------------------------ COMPILE AND SERIALIZE VALIDATOR ------------------------------------

{-# INLINABLE  mkWrappedPolicy #-}
mkWrappedPolicy :: MintParams -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicy = wrapPolicy . mkPolicy

policy :: MintParams -> MintingPolicy
policy np = mkMintingPolicyScript $
    $$(compile [|| mkWrappedPolicy ||])
    `applyCode`
    liftCode np

script :: MintParams -> Script
script = unMintingPolicyScript . policy

validator :: MintParams -> Validator
validator = Validator . script

scriptAsCbor :: MintParams -> LBS.ByteString
scriptAsCbor = serialise . validator

apiMintScript :: MintParams -> PlutusScript PlutusScriptV2
apiMintScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . scriptAsCbor
