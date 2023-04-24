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
import           Plutus.Model           (Ada (Lovelace), DatumMode (InlineDatum),
                                         Run, Tx, TypedValidator (TypedValidator),
                                         UserSpend, ada, adaValue,
                                         defaultBabbage, logError, mustFail,
                                         newUser, payToKey, payToScript, spend, submitTx, testNoErrors,
                                         toV2, userSpend, utxoAt,
                                         valueAt, TypedPolicy (TypedPolicy), mintValue, spendPubKey, scriptCurrencySymbol, datumAt,
                                          spendScript, signTx, refInputInline)
import           Plutus.V2.Ledger.Api   (PubKeyHash,
                                         TxOut (txOutValue), TxOutRef, Value, singleton, TokenName)
import           PlutusTx.Builtins      (Integer)
import           PlutusTx.Prelude       (Eq ((==)), ($), (.), Maybe (Just), negate)
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
      ]
    where
      bad msg = good msg . mustFail
      good = testNoErrors (adaValue 10_000_000_000) defaultBabbage

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

-- Set many users at once
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 2 $ newUser $ ada (Lovelace 1_000_000_000)

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
  [u1, _] <- setupUsers
  void $ mintNFT u1

testMintNFTTwice :: Run ()
testMintNFTTwice = do
  [u1, _] <- setupUsers
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
  [u1,_] <- setupUsers
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

updateOracle :: PubKeyHash -> Oracle.Rate ->  Oracle.Rate -> OracleValidator -> AssetClass -> Run OracleValidator
updateOracle u last new ov nftAC = do
  [(ref,_)] <- utxoAt ov
  let oracle = Oracle.OracleParams nftAC u
      oracleTx = updateOracleTx oracle last new ref (assetClassValue nftAC 1)
  signed <- signTx u oracleTx
  submitTx u signed
  return $ oracleScript oracle

testUpdateOracle :: Run ()
testUpdateOracle = do
  [u1, _] <- setupUsers
  -- Deploy Oracle
  (ov, ac) <- deployOracle u1 25
  -- Update Oracle
  ov' <-  updateOracle u1 25 26 ov ac
  -- Check that the oracle updated correctly
  [(ref,_)] <- utxoAt ov'
  dat <- datumAt ref :: Run (Maybe Oracle.Rate)
  case dat of
    Just r -> unless (r == 26) $ logError "Datum doesn't match!"
    _ -> logError "Oracle is not deployed correctly: Could not find datum"

testUpdateOracleWrongSigner :: Run ()
testUpdateOracleWrongSigner = do
  [u1, u2] <- setupUsers
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

mintStablecoinTx :: PubKeyHash
                 -> TypedPolicy Minting.MintRedeemer
                 -> Value
                 -> TxOutRef
                 -> Collateral.CollateralDatum
                 -> UserSpend
                 -> Tx
mintStablecoinTx u pol val ref dat us = mconcat
                 [ mintValue pol Minting.Mint val
                 , payToKey u val
                 , refInputInline ref
                 , payToScript collateralScript (InlineDatum dat) (adaValue 3000000)
                 , userSpend us
                 ]

burnStablecoinTx :: UserSpend -> PubKeyHash -> TypedPolicy Minting.MintRedeemer -> TxOutRef -> Collateral.CollateralDatum -> Value -> Tx
burnStablecoinTx us user policy ref dat burnVal = mconcat
                   [ spendScript collateralScript ref Collateral.Redeem dat
                   , payToKey user (adaValue 3000000)
                   , mintValue policy Minting.Burn (negate burnVal)
                   , userSpend us
                   ]

mintStablecoin :: TxOutRef -> PubKeyHash -> Value -> Collateral.CollateralDatum -> Oracle.OracleParams -> Run ()
mintStablecoin oRef user mintingVal datum op = do
  sp <- spend user $ adaValue 3000000
  let oracleVH = validatorHash' $ Oracle.validator op

      -- Get Collateral validatorhash
      collateralVH = validatorHash' Collateral.validator
      -- Get Stablecoin minting policy
      stablecoinMP = stableCoinScript $ Minting.MintParams oracleVH collateralVH
      tx = mintStablecoinTx user stablecoinMP mintingVal oRef datum sp
  submitTx user tx

burnStablecoin :: TxOutRef -> TypedPolicy Minting.MintRedeemer -> Collateral.CollateralDatum -> PubKeyHash -> Value -> Run ()
burnStablecoin oRef policy dat user value = do
  us <- spend user value
  let tx = burnStablecoinTx us user policy oRef dat value
  submitTx user tx

testE2E :: Run ()
testE2E = do
  [u1,u2] <- setupUsers
  -- Deploy Oracle
  (ov, ac) <- deployOracle u1 200
  let amountToMint = 2
      oracleParams = Oracle.OracleParams ac u1
      oracleVH     = validatorHash' $ Oracle.validator oracleParams
      collateralVH = validatorHash' Collateral.validator

      stablecoinMP = stableCoinScript $ Minting.MintParams oracleVH collateralVH
      currSymbol = scriptCurrencySymbol stablecoinMP
      datum = Collateral.CollateralDatum currSymbol u2 amountToMint Collateral.Locked
      mintingValue = singleton currSymbol Collateral.stablecoinTokenName amountToMint

  -- Update Oracle
  ov' <-  updateOracle u1 200 100 ov ac
  [(ref,_)] <- utxoAt ov'
  -- Mint stablecoin
  mintStablecoin ref u2 mintingValue datum oracleParams
  [(colRef,_)] <- utxoAt collateralScript
  -- Burn stablecoin
  burnStablecoin colRef stablecoinMP datum u2 mintingValue

testMintStableCoin :: Run ()
testMintStableCoin = do
  [u1,u2] <- setupUsers
  -- Deploy Oracle
  (ov, ac) <- deployOracle u1 200
  -- Update Oracle
  ov' <-  updateOracle u1 200 100 ov ac
  [(ref,_)] <- utxoAt ov'
      -- get Oracle validatorHash

  sp <- spend u1 $ adaValue 3000000
  let oracleVH = validatorHash' $ Oracle.validator $ Oracle.OracleParams ac u1

      -- get Collateral validatorhash
      collateralVH = validatorHash' Collateral.validator

      stablecoinMP = stableCoinScript $ Minting.MintParams oracleVH collateralVH
      currSymbol = scriptCurrencySymbol stablecoinMP
      datum = Collateral.CollateralDatum currSymbol u1 2 Collateral.Locked
      mintingValue = singleton currSymbol Collateral.stablecoinTokenName 2
      tx = mintStablecoinTx u1 stablecoinMP mintingValue ref datum sp
  submitTx u1 tx