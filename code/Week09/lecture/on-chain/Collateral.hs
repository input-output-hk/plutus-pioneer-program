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
                                   ScriptContext(scriptContextTxInfo),
                                   TxInfo(txInfoMint),
                                   BuiltinData,
                                   PubKeyHash,
                                   CurrencySymbol,
                                   TokenName(TokenName) )
import Plutus.V1.Ledger.Value    ( assetClassValueOf, AssetClass(AssetClass))
import Plutus.V2.Ledger.Contexts ( findDatum, txSignedBy )
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
                                   Eq(..) )
import           Utilities        (wrapValidator, writeValidatorToFile)
import qualified Prelude


---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: HELPER FUNCTIONS/TYPES ------------------------------------

stablecoinTokenName :: TokenName 
stablecoinTokenName = TokenName $ encodeUtf8 "USDP"

{-# INLINABLE parseCollateralDatum #-}
parseCollateralDatum :: OutputDatum -> TxInfo -> Maybe CollateralDatum
parseCollateralDatum o info = case o of
    NoOutputDatum         -> traceError "Found Collateral output but NoOutputDatum"
    OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d
    OutputDatumHash dh    -> do
                           Datum d <- findDatum dh info
                           PlutusTx.fromBuiltinData d

---------------------------------------------------------------------------------------------------
------------------------------------ ON-CHAIN: VALIDATOR ------------------------------------------

-- Datum containing all the relevant information 
data CollateralDatum = CollateralDatum
    { colMintingPolicyId  :: CurrencySymbol 
    , colOwner            :: PubKeyHash
    , colStablecoinAmount :: Integer
    } deriving Prelude.Show
unstableMakeIsData ''CollateralDatum

-- We can lock or redeem our own collateral or liquidate someone else's
data CollateralRedeemer = Redeem | Liquidate
unstableMakeIsData ''CollateralRedeemer


{-# INLINABLE mkValidator #-}
mkValidator :: CollateralDatum -> CollateralRedeemer -> ScriptContext -> Bool
mkValidator dat r ctx = case r of
    Redeem    -> traceIfFalse "collateral owner's signature missing" checkSignedByCollOwner &&
                 traceIfFalse "burned stablecoin amount mismatch" checkStablecoinAmount

    Liquidate -> traceIfFalse "burned stablecoin amount mismatch" checkStablecoinAmount

  where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Check if the transaction is signed by the collateral owner
    checkSignedByCollOwner :: Bool
    checkSignedByCollOwner = txSignedBy info $ colOwner dat

    -- Amount of stablecoins minted in this transaction
    mintedAmount :: Integer
    mintedAmount = assetClassValueOf (txInfoMint info) (AssetClass (colMintingPolicyId dat, stablecoinTokenName))

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
