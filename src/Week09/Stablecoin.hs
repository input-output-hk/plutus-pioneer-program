{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Week09.Stablecoin where

import           GHC.Generics                (Generic)
import           PlutusLedgerApi.Common      (SerialisedScript,
                                              serialiseCompiledCode)
import           PlutusLedgerApi.V1.Address  (scriptHashAddress)
import           PlutusLedgerApi.V1.Value    (AssetClass (..),
                                              CurrencySymbol (unCurrencySymbol),
                                              assetClassValueOf, valueOf)
import           PlutusLedgerApi.V3          (Datum (..), OutputDatum (..),
                                              PubKeyHash, Redeemer (..),
                                              ScriptContext (..),
                                              ScriptHash (ScriptHash),
                                              ScriptInfo (MintingScript, SpendingScript),
                                              TokenName (TokenName),
                                              TxInInfo (..), TxInfo (..),
                                              TxOut (..), Value, adaSymbol,
                                              adaToken)
import           PlutusLedgerApi.V3.Contexts (findDatum, getContinuingOutputs,
                                              ownCurrencySymbol, txSignedBy)
import           PlutusTx                    (BuiltinData, CompiledCode,
                                              FromData (..),
                                              UnsafeFromData (unsafeFromBuiltinData),
                                              compile, makeIsDataSchemaIndexed,
                                              makeLift)
import           PlutusTx.Blueprint          (HasBlueprintDefinition,
                                              definitionRef)
import           PlutusTx.Bool               (Bool (..), (&&))
import           PlutusTx.Prelude            (BuiltinUnit, Eq (..), Integer,
                                              Maybe (..), Ord (..), check,
                                              divide, encodeUtf8, negate,
                                              traceError, traceIfFalse, ($),
                                              (*), (.))
import           Week09.Oracle               (parseOracleDatum)

{- ------------------------------------------------------------------------------------------ -}
{- ----------------------------------------- TYPES ------------------------------------------ -}

{-
NOTE: spCollateralMinPercent = 150 means that the value of the locked collateral has to be
at least 150% of the minted amount value
-}

data StablecoinParams = StablecoinParams
  { spOracleValidator      :: ScriptHash
  , spCollateralMinPercent :: Integer
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

makeLift ''StablecoinParams
makeIsDataSchemaIndexed ''StablecoinParams [('StablecoinParams, 0)]

-- We can mint or burn our own stablecoins and liquidate someone else's.
data StablecoinRedeemer = Mint | Burn | Liquidate
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''StablecoinRedeemer [('Mint, 0), ('Burn, 1), ('Liquidate, 2)]

-- Datum containing all the relevant information
data StablecoinDatum = StablecoinDatum
  { colOwner            :: PubKeyHash
  , colStablecoinAmount :: Integer
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''StablecoinDatum [('StablecoinDatum, 0)]

{- ------------------------------------------------------------------------------------------ -}
{- --------------------------- ON-CHAIN: HELPER FUNCTIONS/TYPES ----------------------------- -}

stablecoinTokenName :: TokenName
stablecoinTokenName = TokenName $ encodeUtf8 "USDP"

{-# INLINEABLE parseStablecoinDatum #-}
parseStablecoinDatum :: OutputDatum -> TxInfo -> Maybe StablecoinDatum
parseStablecoinDatum o info = case o of
  NoOutputDatum -> traceError "Found Collateral output but NoOutputDatum"
  OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d
  OutputDatumHash dh -> do
    Datum d <- findDatum dh info
    PlutusTx.fromBuiltinData d

{- ------------------------------------------------------------------------------------------ -}
{- --------------------------------------- VALIDATOR ---------------------------------------- -}

{- NOTE: I'm not taking advatage of CIP-69 to embed NFT logic in here. Should I? 😶 I don't think
 - it would help much. It might even be detrimental because I'd use a oneshot logic that would
 - then stick around forever without any use.
 - -}

{-# INLINEABLE stablecoinVal #-}
stablecoinVal :: StablecoinParams -> ScriptContext -> Bool
stablecoinVal params ctx = case scriptContextScriptInfo ctx of
  MintingScript currSymbol -> case r of
    Mint ->
      traceIfFalse "minted amount must be positive" checkMintPositive
        && traceIfFalse "minted amount exceeds max" checkMaxMintOut
        && traceIfFalse "invalid datum at collateral output" checkDatum
    Burn ->
      traceIfFalse "invalid burning amount" (checkBurnAmountMatchesColDatum currSymbol)
        && traceIfFalse "owner's signature missing" (checkColOwner currSymbol)
        && traceIfFalse "Minting instead of burning!" checkBurnNegative
    Liquidate ->
      traceIfFalse "invalid liquidating amount" (checkBurnAmountMatchesColDatum currSymbol)
        && traceIfFalse "liquidation threshold not reached" (checkLiquidation currSymbol)
        && traceIfFalse "Minting instead of burning!" checkBurnNegative
  SpendingScript _txOutRef (Just d) -> case (fromBuiltinData @StablecoinDatum . getDatum) d of
    Just dat -> case r of
      Mint -> traceError "No need to spend an utxo to mint stablecoins"
      Burn ->
        traceIfFalse "collateral owner's signature missing" (checkSignedByCollOwner dat)
          && traceIfFalse "burned stablecoin amount mismatch" (checkStablecoinAmount dat)
      Liquidate -> traceIfFalse "burned stablecoin amount mismatch" (checkStablecoinAmount dat)
    Nothing -> traceError "Expected correctly shaped datum"
  _ -> False
 where
  r = case fromBuiltinData $ getRedeemer (scriptContextRedeemer ctx) of
    Just @StablecoinRedeemer n -> n
    Nothing                    -> traceError "Redeemer has the wrong shape"

  info :: TxInfo
  info = scriptContextTxInfo ctx

  --------- ORACLE-RELATED FUNCTIONS ------------

  -- Get the oracle's input
  getOracleInput :: TxOut
  ~getOracleInput = case oracleInputs of
    [o] -> o
    _   -> traceError "expected exactly one oracle input"
   where
    oracleInputs :: [TxOut]
    oracleInputs =
      [ o
      | i <- txInfoReferenceInputs info
      , let o = txInInfoResolved i
      , txOutAddress o == scriptHashAddress (spOracleValidator params)
      ]

  -- Get the rate (Datum) from the Oracle
  rate :: Integer
  ~rate = case parseOracleDatum getOracleInput info of
    Nothing -> traceError "Oracle's datum not found"
    Just x  -> x

  --------- MINTING-RELATED FUNCTIONS ------------

  -- Get amount of stablecoins to minted (or burned if negative) in this transaction
  mintedAmount :: Integer
  ~mintedAmount = assetClassValueOf (txInfoMint info) (AssetClass (ownCurrencySymbol ctx, stablecoinTokenName))

  -- Check that the amount of stablecoins minted is positive
  checkMintPositive :: Bool
  ~checkMintPositive = mintedAmount > 0

  -- Check that the amount of stablecoins burned is negative
  checkBurnNegative :: Bool
  ~checkBurnNegative = mintedAmount < 0

  {-
  maxMint calculates the maximum amount of stablecoins that can be minted with the given collateral.

  Oracle has ada price in USD cents [USD¢] ($1 is ¢100 in the oracle's datum). So rate needs to be divided by 100.
  Also, collateralOutputAmount is in lovelaces [L], so final calculation needs to be divided by 1_000_000.

  ca = collAmount
  CMP = spCollateralMinPercent

                    ca [L]        rate [USD¢/ADA]                 ca [L]
               --------------- * ------------------           --------------- * rate [USD/ADA]
                    CMP [%]        100 [USD¢/USD]                   CMP
                  ---------
                    100 [%]
  maxMint = ------------------------------------------- =  ------------------------------------- = [USD]
                      1_000_000 [L/A]                                1_000_000 [L/A]

  -}
  maxMint :: Integer -> Integer
  maxMint collAmount = (collAmount `divide` spCollateralMinPercent params * rate) `divide` 1_000_000

  -- Check that the amount of stablecoins minted does not exceed the maximum
  checkMaxMintOut :: Bool
  ~checkMaxMintOut = maxMint collateralOutputAmount >= mintedAmount

  --------- COLLATERAL-RELATED FUNCTIONS ------------

  -- Check if the transaction is signed by the collateral owner
  checkSignedByCollOwner :: StablecoinDatum -> Bool
  checkSignedByCollOwner dat = txSignedBy info $ colOwner dat

  -- Check that the amount of stablecoins burned matches the amont at the collateral's datum
  checkStablecoinAmount :: StablecoinDatum -> Bool
  checkStablecoinAmount dat = negate (colStablecoinAmount dat) == mintedAmount

  -- Get the collateral's output datum and value
  collateralOutput :: (OutputDatum, Value)
  collateralOutput = case getContinuingOutputs ctx of
    [TxOut{txOutDatum, txOutValue}] -> (txOutDatum, txOutValue)
    _ -> traceError "expected exactly one collateral output"

  -- Get the collateral's output datum
  collateralOutputDatum :: Maybe StablecoinDatum
  ~collateralOutputDatum = parseStablecoinDatum d info
   where
    (d, _) = collateralOutput

  -- Get the collateral's output amount as an integer
  collateralOutputAmount :: Integer
  ~collateralOutputAmount = valueOf v adaSymbol adaToken
   where
    (_, v) = collateralOutput

  -- Check that the collateral's output datum has the correct values
  checkDatum :: Bool
  ~checkDatum = case collateralOutputDatum of
    Nothing -> False
    Just d ->
      colStablecoinAmount d
        == mintedAmount
        && txSignedBy info (colOwner d)

  -- Get the collateral's input
  collateralInput :: CurrencySymbol -> TxOut
  collateralInput cs = case collateralInputs of
    [o] -> o
    _   -> traceError "expected exactly one collateral input"
   where
    collateralInputs =
      [ o
      | i <- txInfoInputs info
      , let o = txInInfoResolved i
      , txOutAddress o == scriptHashAddress (ScriptHash . unCurrencySymbol $ cs)
      ]

  -- Get the collateral's input datum
  collateralInputDatum :: CurrencySymbol -> Maybe StablecoinDatum
  collateralInputDatum cs = parseStablecoinDatum (txOutDatum $ collateralInput cs) info

  -- Get the collateral's input amount
  collateralInputAmount :: CurrencySymbol -> Integer
  collateralInputAmount cs = valueOf (txOutValue $ collateralInput cs) adaSymbol adaToken

  -- Check that the amount of stablecoins burned matches the amont at the collateral's datum
  checkBurnAmountMatchesColDatum :: CurrencySymbol -> Bool
  checkBurnAmountMatchesColDatum cs = case collateralInputDatum cs of
    Nothing -> False
    Just d  -> negate (colStablecoinAmount d) == mintedAmount

  -- Check that the owner's signature is present
  checkColOwner :: CurrencySymbol -> Bool
  checkColOwner cs = case collateralInputDatum cs of
    Nothing -> False
    Just d  -> txSignedBy info (colOwner d)

  -- Check that the collateral's value is low enough to liquidate
  checkLiquidation :: CurrencySymbol -> Bool
  checkLiquidation cs = maxMint (collateralInputAmount cs) < negate mintedAmount

{- ------------------------------------------------------------------------------------------ -}
{- ---------------------------------------- HELPERS ----------------------------------------- -}

compiledVal :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
compiledVal = $$(compile [||wrappedVal||])
 where
  wrappedVal :: BuiltinData -> BuiltinData -> BuiltinUnit
  wrappedVal params ctx = PlutusTx.Prelude.check $ stablecoinVal (unsafeFromBuiltinData params) (unsafeFromBuiltinData ctx)

serializedStablecoinVal :: SerialisedScript
serializedStablecoinVal = serialiseCompiledCode compiledVal
