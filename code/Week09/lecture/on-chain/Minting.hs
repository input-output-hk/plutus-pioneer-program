{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}


module Minting where

import Plutus.V2.Ledger.Api      ( OutputDatum(..), TxOut(txOutAddress, txOutValue),
                                   Value, ScriptContext(scriptContextTxInfo),
                                   TxInfo(txInfoReferenceInputs, txInfoMint),
                                   BuiltinData, mkMintingPolicyScript, adaToken,
                                   adaSymbol, MintingPolicy, TxInInfo(txInInfoResolved),
                                   txInfoInputs, txOutDatum, UnsafeFromData (unsafeFromBuiltinData),
                                   ValidatorHash, txOutValue)
import Plutus.V1.Ledger.Value    ( assetClassValueOf, AssetClass(AssetClass), valueOf )
import Plutus.V1.Ledger.Address  ( scriptHashAddress )
import Plutus.V2.Ledger.Contexts ( txSignedBy, scriptOutputsAt, ownCurrencySymbol )
import PlutusTx                  ( compile, unstableMakeIsData,
                                   liftCode, applyCode, makeLift, CompiledCode )
import PlutusTx.Prelude          ( Bool(False), Integer, Maybe(..), (.), negate, traceError,
                                   (&&), traceIfFalse, ($), Ord((<), (>), (>=)), Eq((==)), divide,
                                   MultiplicativeSemigroup((*)))
import qualified Prelude         ( Show, IO)
import           Oracle          ( parseOracleDatum)
import           Collateral      ( CollateralDatum (..), stablecoinTokenName, parseCollateralDatum)
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
    Mint      -> traceIfFalse "minted amount must be positive" checkMintPositive &&
                 traceIfFalse "minted amount exceeds max" checkMaxMintOut &&
                 traceIfFalse "invalid datum at collateral output" checkDatum

    Burn      -> traceIfFalse "invalid burning amount" checkBurnAmountMatchesColDatum &&
                 traceIfFalse "owner's signature missing" checkColOwner &&
                 traceIfFalse "Minting instead of burning!" checkBurnNegative

    Liquidate -> traceIfFalse "invalid liquidating amount" checkBurnAmountMatchesColDatum &&
                 traceIfFalse "liquidation threshold not reached" checkLiquidation &&
                 traceIfFalse "Minting instead of burning!" checkBurnNegative
                 
    where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    --------- ORACLE-RELATED FUNCTIONS ------------

    -- Get the oracle's input
    getOracleInput :: TxOut
    getOracleInput = case oracleInputs of
                    [o] -> o
                    _   -> traceError "expected exactly one oracle input"
        where
            oracleInputs :: [TxOut]
            oracleInputs = [ o
                           | i <- txInfoReferenceInputs info
                           , let o = txInInfoResolved i
                           , txOutAddress o == scriptHashAddress (mpOracleValidator mp)
                           ]

    -- Get the rate (Datum) from the Oracle
    rate :: Integer
    rate = case parseOracleDatum getOracleInput info of
        Nothing -> traceError "Oracle's datum not found"
        Just x  -> x


    --------- MINTING-RELATED FUNCTIONS ------------

    -- Get amount of stablecoins to minted (or burned if negative) in this transaction
    mintedAmount :: Integer
    mintedAmount = assetClassValueOf (txInfoMint info) (AssetClass (ownCurrencySymbol ctx, stablecoinTokenName))

    -- Check that the amount of stablecoins minted is positive
    checkMintPositive :: Bool
    checkMintPositive = mintedAmount > 0

    -- Check that the amount of stablecoins burned is negative
    checkBurnNegative :: Bool
    checkBurnNegative = mintedAmount < 0

    {-
    maxMint calculates the maximum amount of stablecoins that can be minted with the given collateral.

    Oracle has ada price in USD cents [USD¢] ($1 is ¢100 in the oracle's datum). So rate needs to be divided by 100.
    Also, collateralOutputAmount is in lovelaces [L], so final calculation needs to be divided by 1_000_000.

    ca = collAmount
    CMP = mpCollateralMinPercent


                      ca [L]        rate [USD¢/ADA]                 ca [L]       
                 --------------- * ------------------           --------------- * rate [USD/ADA]
                      CMP [%]        100 [USD¢/USD]                   CMP                         
                    ---------                                               
                      100 [%]                                               
    maxMint = ------------------------------------------- =  ------------------------------------- = [USD]
                        1_000_000 [L/A]                                1_000_000 [L/A]
    
    -}
    maxMint :: Integer -> Integer
    maxMint collAmount = (collAmount `divide` mpCollateralMinPercent mp * rate) `divide` 1_000_000

    -- Check that the amount of stablecoins minted does not exceed the maximum
    checkMaxMintOut :: Bool
    checkMaxMintOut = maxMint collateralOutputAmount >= mintedAmount

    --------- COLLATERAL-RELATED FUNCTIONS ------------

    -- Get the collateral's output datum and value
    collateralOutput :: (OutputDatum, Value)
    collateralOutput = case scriptOutputsAt (mpCollateralValidator mp) info of
                        [(h, v)] -> (h, v)
                        _        -> traceError "expected exactly one collateral output"

    -- Get the collateral's output datum
    collateralOutputDatum :: Maybe CollateralDatum
    collateralOutputDatum = parseCollateralDatum d info
        where
            (d,_) = collateralOutput

    -- Get the collateral's output amount as an integer
    collateralOutputAmount :: Integer
    collateralOutputAmount = valueOf v adaSymbol adaToken
        where
            (_,v) = collateralOutput

    -- Check that the collateral's output datum has the correct values
    checkDatum :: Bool
    checkDatum = case collateralOutputDatum of
        Nothing -> False
        Just d  -> colMintingPolicyId d  == ownCurrencySymbol ctx &&
                   colStablecoinAmount d == mintedAmount &&
                   txSignedBy info (colOwner d)

    -- Get the collateral's input
    collateralInput :: TxOut
    collateralInput = case collateralInputs of
                        [o] -> o
                        _   -> traceError "expected exactly one collateral input"
        where
            collateralInputs = [ o
                                | i <- txInfoInputs info
                                , let o = txInInfoResolved i
                                , txOutAddress o == scriptHashAddress (mpCollateralValidator mp)
                                ]

    -- Get the collateral's input datum
    collateralInputDatum :: Maybe CollateralDatum
    collateralInputDatum = parseCollateralDatum (txOutDatum collateralInput) info

    -- Get the collateral's input amount
    collateralInputAmount :: Integer
    collateralInputAmount = valueOf (txOutValue collateralInput) adaSymbol adaToken

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
    checkLiquidation = maxMint collateralInputAmount < negate mintedAmount

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
saveMintingCode = writeCodeToFile "assets/minting.plutus" policyCodeLucid
