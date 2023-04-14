{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

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
                                         valueAt, TypedPolicy (TypedPolicy), mintValue, spendPubKey, scriptCurrencySymbol, datumAt)
import           Plutus.V2.Ledger.Api   (PubKeyHash,
                                         TxOut (txOutValue), TxOutRef, Value, singleton, TokenName)
import           PlutusTx.Builtins      (Integer)
import           PlutusTx.Prelude       (Eq ((==)), ($), (.), Maybe (Just))
import           Prelude                (IO, mconcat, Semigroup ((<>)), undefined)
import           Test.Tasty             (defaultMain, testGroup)
import           Plutus.V1.Ledger.Value (assetClass, AssetClass (AssetClass))

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

main :: IO ()
main = defaultMain $ do
    testGroup
      "Testing validator with some sensible values"
      [ good "Minting NFT works               " testMintNFT
      , bad  "Minting the same NFT twice fails" testMintNFTTwice
      , good "Deploying the Oracle works      " testDeployOracle
      ]
    where
      bad msg = good msg . mustFail
      good = testNoErrors (adaValue 10_000_000) defaultBabbage

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

-- Set many users at once
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 2 $ newUser $ ada (Lovelace 1000)

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
  unless (v1 == adaValue 1000 <> mintingValue) $ 
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
  unless (v1 == adaValue 1000 <> mintingValue) $ 
    logError "Final balances are incorrect"

---------------------------------------------------------------------------------------------------
---------------------------------------- TESTING ORACLE -------------------------------------------

type OracleValidator = TypedValidator Integer Oracle.OracleRedeemer

-- Oracle's script
oracleScript :: Oracle.OracleParams -> OracleValidator
oracleScript oracle = TypedValidator . toV2 $ Oracle.validator oracle

deployOracleTx :: UserSpend -> Oracle.OracleParams -> Oracle.Ratio -> Tx
deployOracleTx sp op dat =
  mconcat
    [ userSpend sp
    , payToScript (oracleScript op) (InlineDatum dat) (adaValue 1)
    ]

deployOracle :: Oracle.Ratio -> Run OracleValidator
deployOracle or = do
  [u1, _] <- setupUsers
  -- Mint NFT
  AssetClass (cs,tn) <- mintNFT u1
  -- Deploy Oracle
  sp <- spend u1 $ adaValue 1
  let nftAC = assetClass cs tn
      oracle = Oracle.OracleParams nftAC u1
      oracleTx = deployOracleTx sp oracle or
  submitTx u1 oracleTx
  return $ oracleScript oracle

testDeployOracle :: Run ()
testDeployOracle = do
  -- Deploy Oracle
  ov <- deployOracle 25
  -- Check that the oracle deployed correctly
  [(ref,_)] <- utxoAt ov
  dat <- datumAt ref :: Run (Maybe Oracle.Ratio)
  case dat of
    Just r -> unless (r == 25) $ logError "Datum doesn't match!"
    _ -> logError "Oracle is not deployed correctly: Could not find datum"


testUpdateOracle :: Run ()
testUpdateOracle = do
  -- Deploy Oracle
  ov <- deployOracle 25
  -- Update Oracle
  undefined -- TODO
