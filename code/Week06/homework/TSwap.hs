{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Plutus.Model
import qualified Swap as OC
import           PlutusTx.Prelude     (($))
import           Prelude             (IO, (.), mconcat, (<>), Integer)
import           Test.Tasty           ( defaultMain, testGroup, TestTree )
import           Plutus.V2.Ledger.Api
import qualified Mint as Mint

---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------

-- | Test the validator script
main :: IO ()
main = do
  defaultMain $ do
    testGroup
      "Catch double spend with testing"
      [ homework1 defaultBabbage
      ]

type HomeworkScript = TypedValidator OC.DatumSwap ()
type FakePolicy = TypedPolicy ()

fakePolicy :: TokenName -> FakePolicy
fakePolicy = TypedPolicy . toV2 . Mint.fakeMintingPolicy

swapScript :: HomeworkScript
swapScript = TypedValidator $ toV2 $ OC.validator


homework1 :: MockConfig -> TestTree
homework1 cfg = do
  testGroup "Testing double spending"
    [ good "Normal spending" normalSpending
    , bad "Double spending" doubleSpending
    ]

 where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000) cfg

normalSpending :: Run ()
normalSpending = do
  user1 <- newUser $ ada (Lovelace 1000)
  user2 <- newUser $ ada (Lovelace 1000)
  let policy = fakePolicy "tn"
      mintingValue = singleton (scriptCurrencySymbol policy) "tn" 10
  checkBalance (owns swapScript mintingValue ) $ submitTx user1 $ mintingTx policy mintingValue user1 100
  waitNSlots 10
  utxosU2 <- utxoAt user2
  utxos <- utxoAt swapScript
  let [(ref,_)] = utxos
      [(refU2,_)] = utxosU2
  submitTx user2 $ normalRetrieve user1 user2 ref refU2 100 mintingValue

doubleSpending :: Run ()
doubleSpending = do
  user1 <- newUser $ ada (Lovelace 1000)
  user2 <- newUser $ ada (Lovelace 1000)
  let policy = fakePolicy "tn"
      mintingValue = singleton (scriptCurrencySymbol policy) "tn" 10
  checkBalance (owns swapScript mintingValue ) $ submitTx user1 $ mintingTx policy mintingValue user1 100
  waitNSlots 10
  checkBalance (owns swapScript mintingValue ) $ submitTx user2 $ mintingTx policy mintingValue user1 100
  waitNSlots 10
  utxosU2 <- utxoAt user2
  utxos <- utxoAt swapScript
  let [(ref1,_),(ref2,_)] = utxos
      [(refU2,_)] = utxosU2
  submitTx user2 $ doubleRetrieve user1 user2 ref1 ref2 refU2 100 mintingValue

mintingTx :: (TypedPolicy ()) -> Value -> PubKeyHash -> Integer -> Tx
mintingTx tp val pkh price =
  mconcat
    [ mintValue tp () val
    , payToScript swapScript (HashDatum (OC.DatumSwap {OC.beneficiary = pkh, OC.price = price})) val
    ]

doubleRetrieve :: PubKeyHash -> PubKeyHash -> TxOutRef -> TxOutRef -> TxOutRef -> Integer -> Value -> Tx
doubleRetrieve pkh pkh2 txOut1 txOut2 txOutU2 price val = mconcat
  [ spendScript swapScript txOut1 () dat
  , spendScript swapScript txOut2 () dat
  , payToKey pkh  $ ada (Lovelace price)
  , payToKey pkh2 $ val <> val <> ada (Lovelace 900)
  , spendPubKey txOutU2
  ]
    where
      dat = (OC.DatumSwap {OC.beneficiary = pkh, OC.price = price})

normalRetrieve :: PubKeyHash -> PubKeyHash -> TxOutRef -> TxOutRef -> Integer -> Value -> Tx
normalRetrieve pkh pkh2 txOut txOutU2 price val = mconcat
  [ spendScript swapScript txOut () dat
  , payToKey pkh  $ ada (Lovelace price)
  , payToKey pkh2 $ val <> ada (Lovelace 900)
  , spendPubKey txOutU2
  ]
    where
      dat = (OC.DatumSwap {OC.beneficiary = pkh, OC.price = price})
