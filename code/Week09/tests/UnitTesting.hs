{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main where

import qualified NFT
import qualified Oracle
import qualified Collateral
import qualified Minting
import           Control.Monad          (replicateM, unless, Monad (return), void)
import           Plutus.Model           (Ada (Lovelace), DatumMode (..),
                                         Run, Tx, TypedValidator (TypedValidator),
                                         UserSpend, ada, adaValue,
                                         defaultBabbage, logError, mustFail,
                                         newUser, payToKey, payToScript, spend, submitTx, testNoErrors,
                                         toV2, userSpend, utxoAt,
                                         valueAt, TypedPolicy (TypedPolicy), mintValue, spendPubKey, scriptCurrencySymbol, datumAt,
                                          spendScript, signTx, refInputInline)
import           Plutus.V2.Ledger.Api   (PubKeyHash,
                                         TxOut (txOutValue), TxOutRef, Value, singleton,
                                         TokenName, txOutDatum, OutputDatum (..), fromBuiltinData,
                                         getDatum)
import           PlutusTx.Builtins      (Integer)
import           PlutusTx.Prelude       (Eq ((==)), ($), (.), Maybe (..), negate)
import           Prelude                (IO, mconcat, Semigroup ((<>)))
import           Test.Tasty             (defaultMain, testGroup)
import           Plutus.V1.Ledger.Value (assetClass, AssetClass (), assetClassValue)
import           Utilities

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

main :: IO ()
main = defaultMain $ do
    testGroup
      "Testing validator with some sensible values"
      [ good "Minting NFT works               " testMintNFT
      , bad  "Minting the same NFT twice fails" testMintNFTTwice
      , good "Deploying the Oracle works      " testDeployOracle
      , good "Updating the Oracle works       " testUpdateOracle
      , bad  "Bad signer in update            " testUpdateOracleWrongSigner
      , good "User mints stablecoin           " testMintStableCoin
      , good "End to end                      " testE2E
      , good "Liquidation cases               " testLiquidationCases
      ]
    where
      bad msg = good msg . mustFail
      good = testNoErrors (adaValue 10_000_000_000) defaultBabbage

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

-- Set many users at once
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 4 $ newUser $ ada (Lovelace 1_000_000_000)

---------------------------------------------------------------------------------------------------
------------------------------------- TESTING MINTING NFT -----------------------------------------

-- NFT Minting Policy's script
nftScript :: TxOutRef -> TokenName -> TypedPolicy ()
nftScript ref tn = TypedPolicy . toV2 $ NFT.nftPolicy ref tn

mintNFTTx :: TxOutRef -> TxOut -> TokenName -> Value -> PubKeyHash -> Tx
mintNFTTx ref out tn val pkh =
  mconcat
    [ mintValue (nftScript ref tn) () val
    , payToKey pkh $ val <> txOutValue out
    , spendPubKey ref
    ]

mintNFT :: PubKeyHash -> Run AssetClass
mintNFT u = do
  utxos <- utxoAt u
  let [(ref, out)] = utxos                
      currSymbol = scriptCurrencySymbol (nftScript ref "NFT")
      mintingValue = singleton currSymbol "NFT" 1
  submitTx u $ mintNFTTx ref out "NFT" mintingValue u
  v1<- valueAt u
  unless (v1 == adaValue 1000000000 <> mintingValue) $
    logError "Final balances are incorrect"
  return $ assetClass currSymbol "NFT"

testMintNFT :: Run ()
testMintNFT = do
  [u1,_,_,_] <- setupUsers
  void $ mintNFT u1

testMintNFTTwice :: Run ()
testMintNFTTwice = do
  [u1,_,_,_] <- setupUsers
  utxos <- utxoAt u1               
  let [(ref, out)] = utxos                
      mintingValue = singleton (scriptCurrencySymbol (nftScript ref "NFT")) "NFT" 1
      tx = mintNFTTx ref out "NFT" mintingValue u1
  submitTx u1 tx
  submitTx u1 tx
  v1 <- valueAt u1
  unless (v1 == adaValue 1000000000 <> mintingValue) $
    logError "Final balances are incorrect"

---------------------------------------------------------------------------------------------------
---------------------------------------- TESTING ORACLE -------------------------------------------

type OracleValidator = TypedValidator Integer Oracle.OracleRedeemer

-- Oracle's script
oracleScript :: Oracle.OracleParams -> OracleValidator
oracleScript oracle = TypedValidator . toV2 $ Oracle.validator oracle

deployOracleTx :: UserSpend -> Oracle.OracleParams -> Oracle.Rate -> Value -> Tx
deployOracleTx sp op dat val =
  mconcat
    [ userSpend sp
    , payToScript (oracleScript op) (InlineDatum dat) (adaValue 1 <> val)
    ]

deployOracle :: PubKeyHash -> Oracle.Rate-> Run (OracleValidator, AssetClass)
deployOracle u or = do
  -- Mint NFT
  nftAC <- mintNFT u
  -- Deploy Oracle
  let nftV  = assetClassValue nftAC 1
  sp <- spend u $ adaValue 1 <> nftV
  let
      oracle = Oracle.OracleParams nftAC u
      oracleTx = deployOracleTx sp oracle or nftV
  submitTx u oracleTx
  return (oracleScript oracle, nftAC)

testDeployOracle :: Run ()
testDeployOracle = do
  [u1,_,_,_] <- setupUsers
  -- Deploy Oracle
  (ov, _) <- deployOracle u1 25
  -- Check that the oracle deployed correctly
  [(ref,_)] <- utxoAt ov
  dat <- datumAt ref :: Run (Maybe Oracle.Rate)
  case dat of
    Just r -> unless (r == 25) $ logError "Datum doesn't match!"
    _ -> logError "Oracle is not deployed correctly: Could not find datum"


updateOracleTx :: Oracle.OracleParams -> Oracle.Rate -> Oracle.Rate -> TxOutRef -> Value -> Tx
updateOracleTx op last new oRef nft =
  mconcat
    [ spendScript (oracleScript op) oRef Oracle.Update last
    , payToScript (oracleScript op) (InlineDatum new) (adaValue 1 <> nft)
    ]

updateOracle :: PubKeyHash -> Oracle.Rate ->  Oracle.Rate -> OracleValidator -> AssetClass -> Run ()
updateOracle u last new ov nftAC = do
  [(ref,_)] <- utxoAt ov
  let oracle = Oracle.OracleParams nftAC u
      oracleTx = updateOracleTx oracle last new ref (assetClassValue nftAC 1)
  signed <- signTx u oracleTx
  submitTx u signed
  --return $ oracleScript oracle

testUpdateOracle :: Run ()
testUpdateOracle = do
  [u1,_,_,_] <- setupUsers
  -- Deploy Oracle
  (ov, ac) <- deployOracle u1 25
  -- Update Oracle
  updateOracle u1 25 26 ov ac
  -- Check that the oracle updated correctly
  [(ref,_)] <- utxoAt ov
  dat <- datumAt ref :: Run (Maybe Oracle.Rate)
  case dat of
    Just r -> unless (r == 26) $ logError "Datum doesn't match!"
    _ -> logError "Oracle is not deployed correctly: Could not find datum"

testUpdateOracleWrongSigner :: Run ()
testUpdateOracleWrongSigner = do
  [u1,u2,_,_] <- setupUsers
  -- Deploy Oracle
  (ov, ac) <- deployOracle u1 25
  -- Update Oracle
  ov' <-  updateOracleWS u2 u1 25 26 ov ac
  -- Check that the oracle updated correctly
  [(ref,_)] <- utxoAt ov'
  dat <- datumAt ref :: Run (Maybe Oracle.Rate)
  case dat of
    Just r -> unless (r == 26) $ logError "Datum doesn't match!"
    _ -> logError "Oracle is not deployed correctly: Could not find datum"



updateOracleWS :: PubKeyHash -> PubKeyHash -> Oracle.Rate ->  Oracle.Rate -> OracleValidator -> AssetClass -> Run OracleValidator
updateOracleWS signer u last new ov nftAC = do
  [(ref,_)] <- utxoAt ov
  let oracle = Oracle.OracleParams nftAC u
      oracleTx = updateOracleTx oracle last new ref (assetClassValue nftAC 1)
  signed <- signTx signer oracleTx
  submitTx signer oracleTx
  return $ oracleScript oracle


type CollateralValidator = TypedValidator Collateral.CollateralDatum Collateral.CollateralRedeemer

-- Collateral script
collateralScript :: CollateralValidator
collateralScript = TypedValidator . toV2 $ Collateral.validator

-- Stablecoin policy
stableCoinScript :: Minting.MintParams -> TypedPolicy Minting.MintRedeemer
stableCoinScript = TypedPolicy . toV2 . Minting.policy

mintStablecoinTx :: Value
                 -> PubKeyHash
                 -> TypedPolicy Minting.MintRedeemer
                 -> Value
                 -> TxOutRef
                 -> Collateral.CollateralDatum
                 -> UserSpend
                 -> Tx
mintStablecoinTx col u pol val ref dat us = mconcat
                 [ mintValue pol Minting.Mint val
                 , payToKey u val
                 , refInputInline ref
                 , payToScript collateralScript (InlineDatum dat) col
                 , userSpend us
                 ]

burnStablecoinTx :: UserSpend -> PubKeyHash -> TypedPolicy Minting.MintRedeemer -> TxOutRef -> Collateral.CollateralDatum -> Value -> Tx
burnStablecoinTx us user policy ref dat burnVal = mconcat
                   [ spendScript collateralScript ref Collateral.Redeem dat
                   , payToKey user (adaValue 3000000)
                   , mintValue policy Minting.Burn (negate burnVal)
                   , userSpend us
                   ]

liquidateStablecoinTx :: Value -> UserSpend -> PubKeyHash -> TypedPolicy Minting.MintRedeemer -> TxOutRef -> TxOutRef -> Collateral.CollateralDatum -> Value -> Tx
liquidateStablecoinTx col us user policy oracleRef ref dat burnVal = mconcat
                   [ spendScript collateralScript ref Collateral.Liquidate dat
                   , payToKey user col
                   , refInputInline oracleRef
                   , mintValue policy Minting.Liquidate (negate burnVal)
                   , userSpend us
                   ]

mintStablecoin :: Value -> TxOutRef -> PubKeyHash -> Value -> Collateral.CollateralDatum -> Oracle.OracleParams -> Run ()
mintStablecoin col oRef user mintingVal datum op = do

  sp <- spend user col
  let oracleVH = validatorHash' $ Oracle.validator op

      -- Get Collateral validatorhash
      collateralVH = validatorHash' Collateral.validator
      -- Get Stablecoin minting policy
      stablecoinMP = stableCoinScript $ Minting.MintParams oracleVH collateralVH 150
      tx = mintStablecoinTx col user stablecoinMP mintingVal oRef datum sp
  submitTx user tx

liquidateStablecoin :: Value -> TxOutRef -> TxOutRef -> PubKeyHash -> Value -> Collateral.CollateralDatum -> Oracle.OracleParams -> Run ()
liquidateStablecoin col oRef ref user mintingVal datum op = do
  sp <- spend user mintingVal
  let oracleVH = validatorHash' $ Oracle.validator op

      -- Get Collateral validatorhash
      collateralVH = validatorHash' Collateral.validator
      -- Get Stablecoin minting policy
      stablecoinMP = stableCoinScript $ Minting.MintParams oracleVH collateralVH 150
      tx = liquidateStablecoinTx col sp user stablecoinMP oRef ref datum mintingVal
  submitTx user tx

burnStablecoin :: TxOutRef -> TypedPolicy Minting.MintRedeemer -> Collateral.CollateralDatum -> PubKeyHash -> Value -> Run ()
burnStablecoin oRef policy dat user value = do
  us <- spend user value
  let tx = burnStablecoinTx us user policy oRef dat value
  submitTx user tx

testE2E :: Run ()
testE2E = do
  [u1,u2,_,_] <- setupUsers
  -- Deploy Oracle
  (ov, ac) <- deployOracle u1 200
  let amountToMint = 2
      oracleParams = Oracle.OracleParams ac u1
      oracleVH     = validatorHash' $ Oracle.validator oracleParams
      collateralVH = validatorHash' Collateral.validator

      stablecoinMP = stableCoinScript $ Minting.MintParams oracleVH collateralVH 150
      currSymbol = scriptCurrencySymbol stablecoinMP
      datumU1 = Collateral.CollateralDatum currSymbol u1 amountToMint 
      datumU2 = Collateral.CollateralDatum currSymbol u2 amountToMint 
      mintingValue = singleton currSymbol Collateral.stablecoinTokenName amountToMint
      collateral = adaValue 3000000

  -- Update Oracle
  updateOracle u1 200 100 ov ac
  [(ref,_)] <- utxoAt ov
  -- Mint stablecoin
  mintStablecoin collateral ref u1 mintingValue datumU1 oracleParams
  [u1Collateral] <- findCollateralFor u1
  -- Burn stablecoin
  burnStablecoin u1Collateral stablecoinMP datumU1 u1 mintingValue
  -- Liquidate stablecoin
  mintStablecoin collateral ref u1 mintingValue datumU1 oracleParams
  mintStablecoin collateral ref u2 mintingValue datumU2 oracleParams
  [u2Collateral] <- findCollateralFor u2
  -- Update Oracle
  updateOracle u1 200 50 ov ac
  [(ref',_)] <- utxoAt ov
  liquidateStablecoin collateral ref' u2Collateral u1 mintingValue datumU2 oracleParams

testLiquidationCases :: Run ()
testLiquidationCases = do
  [owner,u1,u2,u3] <- setupUsers
  -- Deploy Oracle
  (ov, ac) <- deployOracle owner 100
  let amountToMintU1 = 2
      amountToMintU2 = 4
      amountToMintU3 = 4
      oracleParams = Oracle.OracleParams ac owner
      oracleVH     = validatorHash' $ Oracle.validator oracleParams
      collateralVH = validatorHash' Collateral.validator

      stablecoinMP = stableCoinScript $ Minting.MintParams oracleVH collateralVH 150
      currSymbol = scriptCurrencySymbol stablecoinMP
      datumU1 = Collateral.CollateralDatum currSymbol u1 amountToMintU1
      datumU2 = Collateral.CollateralDatum currSymbol u2 amountToMintU2
      datumU3 = Collateral.CollateralDatum currSymbol u3 amountToMintU3
      mintingValueU1 = singleton currSymbol Collateral.stablecoinTokenName amountToMintU1
      mintingValueU2 = singleton currSymbol Collateral.stablecoinTokenName amountToMintU2
      mintingValueU3 = singleton currSymbol Collateral.stablecoinTokenName amountToMintU3
      collateral1 = adaValue 6000000
      collateral2 = adaValue 6000000
      collateral3 = adaValue 8000000
  [(ref,_)] <- utxoAt ov

  -- Users 1 2 and 3 mint stablecoin
  mintStablecoin collateral1 ref u1 mintingValueU1 datumU1 oracleParams
  mintStablecoin collateral2 ref u2 mintingValueU2 datumU2 oracleParams
  mintStablecoin collateral3 ref u3 mintingValueU3 datumU3 oracleParams

  -- Owner updates the Oracle
  updateOracle owner 100 50 ov ac
  [(ref',_)] <- utxoAt ov
  [u1Collateral] <- findCollateralFor u1
  [u2Collateral] <- findCollateralFor u2

  -- User3 tries to liquidate collateral of user 1 but fails
  mustFail $ liquidateStablecoin collateral1 ref' u1Collateral u3 mintingValueU1 datumU1 oracleParams
  -- User3 tries to liquidate collateral of user2 and succeeds
  liquidateStablecoin collateral2 ref' u2Collateral u3 mintingValueU2 datumU2 oracleParams

findCollateralFor :: PubKeyHash -> Run [TxOutRef]
findCollateralFor user = do
  utxos <- utxoAt collateralScript
  let refs' = [ ref | (ref,o) <- utxos, getOwner o == Just user]
  return refs'

getOwner :: TxOut -> Maybe PubKeyHash
getOwner oRef = case txOutDatum oRef of
   OutputDatum od -> case fromBuiltinData (getDatum od) of
     Nothing              -> Nothing
     Just collateralDatum -> Just $ Collateral.colOwner collateralDatum
   _ -> Nothing

testMintStableCoin :: Run ()
testMintStableCoin = do
  [u1,u2,_,_] <- setupUsers
  -- Deploy Oracle
  (ov, ac) <- deployOracle u1 200
  -- Update Oracle
  updateOracle u1 200 100 ov ac
  [(ref,_)] <- utxoAt ov
      -- get Oracle validatorHash

  sp <- spend u1 $ adaValue 3000000
  let oracleVH = validatorHash' $ Oracle.validator $ Oracle.OracleParams ac u1

      -- get Collateral validatorhash
      collateralVH = validatorHash' Collateral.validator

      stablecoinMP = stableCoinScript $ Minting.MintParams oracleVH collateralVH 150
      currSymbol = scriptCurrencySymbol stablecoinMP
      datum = Collateral.CollateralDatum currSymbol u1 2
      mintingValue = singleton currSymbol Collateral.stablecoinTokenName 2
      collateral = adaValue 3000000
      tx = mintStablecoinTx collateral u1 stablecoinMP mintingValue ref datum sp
  submitTx u1 tx
