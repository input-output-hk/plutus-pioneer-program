{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}


module Minting where

import Plutus.V2.Ledger.Api      ( Datum(Datum), OutputDatum(..), TxOut(txOutAddress),
                                   Value, ScriptContext(scriptContextTxInfo),
                                   TxInfo(txInfoReferenceInputs, txInfoMint),
                                   BuiltinData, mkMintingPolicyScript, adaToken,
                                   adaSymbol, MintingPolicy, TxInInfo(txInInfoResolved),
                                   txInfoInputs, txOutDatum, UnsafeFromData (unsafeFromBuiltinData),
                                   ValidatorHash)
import Plutus.V1.Ledger.Value    ( assetClassValueOf, AssetClass(AssetClass), valueOf )
import Plutus.V1.Ledger.Address  ( scriptHashAddress )
import Plutus.V2.Ledger.Contexts ( findDatum, txSignedBy, scriptOutputsAt, ownCurrencySymbol )
import PlutusTx                  ( compile, unstableMakeIsData, FromData(fromBuiltinData),
                                   liftCode, applyCode, makeLift, CompiledCode )
import PlutusTx.Prelude          ( Bool(False), Integer, Maybe(..), (.), negate, traceError,
                                   (&&), traceIfFalse, ($), Ord((<), (>), (>=)), Eq((==)),
                                   length, divide, MultiplicativeSemigroup((*)))
import qualified Prelude         ( Show, IO)
import           Oracle          ( parseOracleDatum)
import           Collateral      ( CollateralDatum (..), CollateralLock (..), stablecoinTokenName)
import           Utilities       (wrapPolicy, writeCodeToFile)


---------------------------------------------------------------------------------------------------
------------------------------------ ON-CHAIN: VALIDATOR ------------------------------------------

-- Oracle and collateral validator hashes are passed as parameters
data MintParams = MintParams
    { mpOracleValidator      :: ValidatorHash
    , mpCollateralValidator  :: ValidatorHash
    , mpCollateralMinPercent :: Integer
    } deriving Prelude.Show
makeLift ''MintParams

{-
mpCollateralMinPercent = 150 means that the value of the locked collateral has to be at least 150%
of the minted amount value
-}

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

    Burn      -> traceIfFalse "invalid burning amount" checkBurnAmountMatchesColDatum &&
                 traceIfFalse "owner's signature missing" checkColOwner

    Liquidate -> traceIfFalse "invalid liquidating amount" checkBurnAmountMatchesColDatum &&
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

    -- Get the rate (Datum) from the Oracle
    rate :: Integer
    rate = case parseOracleDatum getOracleInput (`findDatum` info) of
        Nothing -> traceError "Oracle's datum not found"
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

    Oracle has ada price in USD cents [USD¢] ($1 is ¢100 in the oracle's datum). So rate needs to be divided by 100.
    Also, collateralAmount is in lovelaces [L], so final calculation needs to be divided by 1_000_000.

    ca = collateralAmount
    CMP = mpCollateralMinPercent


                      ca [L]        rate [USD¢/ADA]                 ca [L]       
                 --------------- * ------------------           --------------- * rate [USD/ADA]
                      CMP [%]        100 [USD¢/USD]                   CMP                         
                    ---------                                               
                      100 [%]                                               
    maxMint = ------------------------------------------- =  ------------------------------------- = [USD]
                        1_000_000 [L/A]                                1_000_000 [L/A]
    
    -}
    maxMint :: Integer
    maxMint = (collateralAmount `divide` mpCollateralMinPercent mp * rate) `divide` 1_000_000

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
                NoOutputDatum         -> traceError "Found Collateral output but NoOutputDatum"
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

    collateralInputs :: [TxOut]
    collateralInputs = [ o
                       | i <- txInfoInputs info
                       , let o = txInInfoResolved i
                       , txOutAddress o == scriptHashAddress (mpCollateralValidator mp)
                       ]

    collateralInputDatum :: Maybe CollateralDatum
    collateralInputDatum = case collateralInputs of
      [o] -> case txOutDatum o of
        NoOutputDatum -> traceError "Found Collateral Input but NoOutputDatum"
        OutputDatum (Datum d) -> fromBuiltinData d
        OutputDatumHash dh    -> do
          Datum d <- findDatum dh info
          fromBuiltinData d
      _  -> traceError "Missing colateral Input"

    -- Check that the amount of stablecoins burned matches the amont at the collateral's datum
    checkBurnAmountMatchesColDatum :: Bool
    checkBurnAmountMatchesColDatum = case collateralInputDatum of
        Nothing -> False
        Just d  -> negate (colStablecoinAmount d) == mintedAmount

    -- Check that the owner's signature is present
    checkColOwner :: Bool
    checkColOwner = case collateralInputDatum of
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

{-# INLINABLE  mkWrappedPolicyLucid #-}
--                     oracle ValHash   coll ValHash   minPercent      redeemer       context
mkWrappedPolicyLucid :: BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicyLucid ov cv p = wrapPolicy $ mkPolicy mp
    where
        mp = MintParams
            { mpOracleValidator  = unsafeFromBuiltinData ov
            , mpCollateralValidator = unsafeFromBuiltinData cv
            , mpCollateralMinPercent = unsafeFromBuiltinData p
            }

policyCodeLucid :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
policyCodeLucid = $$( compile [|| mkWrappedPolicyLucid ||])

saveMintingCode :: Prelude.IO ()
saveMintingCode = writeCodeToFile "assets/minting1.plutus" policyCodeLucid
