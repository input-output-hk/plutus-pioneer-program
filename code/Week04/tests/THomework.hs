{-# LANGUAGE NumericUnderscores #-}

module Main where

import           Control.Monad                 (replicateM)
import           Plutus.Model
import           Plutus.Model.Fork.Ledger.Slot (Slot)
import           Plutus.V2.Ledger.Api
import           Prelude
import           Test.Tasty
import qualified Homework1                     as H1
import qualified Homework2                     as H2

type Homework1Script = TypedValidator H1.MisteryDatum ()

script1 :: Homework1Script
script1 = TypedValidator $ toV2 H1.validator

type Homework2Script = TypedValidator POSIXTime ()

script2 :: PubKeyHash -> Homework2Script
script2 = TypedValidator . toV2 . H2.validator

setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 3 $ newUser $ ada (Lovelace 1000)

main :: IO ()
main = do
  defaultMain $ do
    testGroup "Homework tests"
      [
        testGroup "All times are in POSIXTime (Not slots)"
          [ homework1 defaultBabbage
          , homework2 defaultBabbage
          ]
      ]

homework1 :: MockConfig -> TestTree
homework1 cfg = do
  testGroup
    "Testing Homework1"
    [ testGroup
        "Beneficiary 1 signing"
        [ good "Deadline: 6000; TxValidRange (5000, 5999)" $ testBeneficiary1 6000 (-999) 0 0
        , good "Deadline: 6000; TxValidRange (5000, 6000)" $ testBeneficiary1 6000 (-999) 1 0
        , good "Deadline: 6000; TxValidRange (5000, 6999)" $ testBeneficiary1 6000 (-999) 1000 0
        , good "Deadline: 6000; TxValidRange (5999, 6001)" $ testBeneficiary1 6000 0 2 0
        , good "Deadline: 6000; TxValidRange (6999, 6999)" $ testBeneficiary1 6000 0 0 1
        , bad  "Deadline: 6000; TxValidRange (7000, 8000)" $ testBeneficiary1 6000 1 1001 1
        , bad  "Deadline: 6000; TxValidRange (5000, 7000)" $ testBeneficiary1 6000 (-999) 1001 0
        , bad  "Deadline: 6000; TxValidRange (6000, 7000)" $ testBeneficiary1 6000 (-999) 1 1
        , bad  "Deadline: 6000; TxValidRange (6999, 7000)" $ testBeneficiary1 6000 0 1 1
        ]
    , testGroup
        "Beneficiary 2 signing"
        [ good "Deadline: 5000; TxValidRange (6000, 7000)" $ testBeneficiary2 5000 (-999) 1 1
        , good "Deadline: 4999; TxValidRange (5000, 6000)" $ testBeneficiary2 4999 (-999) 1 0
        , bad  "Deadline: 6000; TxValidRange (5000, 5999)" $ testBeneficiary2 6000 (-999) 0 0
        , bad  "Deadline: 5000; TxValidRange (5000, 6000)" $ testBeneficiary2 5000 (-999) 1 0
        , bad  "Deadline: 5000; TxValidRange (5001, 6000)" $ testBeneficiary2 5000 (-998) 1 0
        , bad  "Deadline: 5000; TxValidRange (5999, 6000)" $ testBeneficiary2 5000 0 1 0
        ]
    , bad "None signing" $ testNoSigning 5000 0 0 0
    ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000) cfg

testBeneficiary1 :: POSIXTime -> POSIXTime -> POSIXTime -> Slot -> Run ()
testBeneficiary1 deadline curMinT curMaxT wSlot = do
  users <- setupUsers
  let [u1, u2, u3] = users
      dat = H1.MisteryDatum u1 u2 deadline
  testHomework1 u1 u3 dat curMinT curMaxT wSlot

testBeneficiary2 :: POSIXTime -> POSIXTime -> POSIXTime -> Slot -> Run ()
testBeneficiary2 deadline curMinT curMaxT wSlot = do
  users <- setupUsers
  let [u1, u2, u3] = users
      dat = H1.MisteryDatum u1 u2 deadline
  testHomework1 u2 u3 dat curMinT curMaxT wSlot

testNoSigning :: POSIXTime -> POSIXTime -> POSIXTime -> Slot -> Run ()
testNoSigning deadline curMinT curMaxT wSlot = do
  users <- setupUsers
  let [u1, u2, u3] = users
      dat = H1.MisteryDatum u1 u2 deadline
  testHomework1 u3 u3 dat curMinT curMaxT wSlot

testHomework1 :: PubKeyHash -> PubKeyHash -> H1.MisteryDatum -> POSIXTime -> POSIXTime -> Slot -> Run ()
testHomework1 sigUser receiver dat curMinT curMaxT wSlot = do
  let val = adaValue 100
  checkBalance (gives sigUser val script1) $ do
    sp <- spend sigUser val
    submitTx sigUser $ misteryTx1 dat sp val
  waitNSlots wSlot
  utxos <- utxoAt script1
  let [(vestRef, vestOut)] = utxos
  checkBalance (gives script1 (txOutValue vestOut) receiver) $ do
    range <- currentTimeInterval curMinT curMaxT
    tx <- validateIn range $ claimingTx1 receiver dat vestRef (txOutValue vestOut)
    submitTx sigUser tx

misteryTx1 :: H1.MisteryDatum -> UserSpend -> Value -> Tx
misteryTx1 dat usp val =
  mconcat
    [ userSpend usp
    , payToScript script1 (HashDatum dat) val
    ]

claimingTx1 :: PubKeyHash -> H1.MisteryDatum -> TxOutRef -> Value -> Tx
claimingTx1 pkh dat vestRef vestVal =
  mconcat
    [ spendScript script1 vestRef () dat
    , payToKey pkh vestVal
    ]


homework2 :: MockConfig -> TestTree
homework2 cfg = do
  testGroup
    "Testing Homework2"
    [ good "Deadline: 5000; TxValidRange (6000, 7000)" $ testHomework2 5000 (-999) 1 1
    , good "Deadline: 5000; TxValidRange (5000, 5000)" $ testHomework2 5000 (-999) (-999) 0
    , good "Deadline: 5000; TxValidRange (5000, 6000)" $ testHomework2 5000 (-999) 1 0
    , good "Deadline: 5000; TxValidRange (5001, 6000)" $ testHomework2 5000 (-998) 1 0
    , good "Deadline: 5000; TxValidRange (5999, 6000)" $ testHomework2 5000 0 1 0
    , bad  "Deadline: 5000; TxValidRange (4000, 5000)" $ testHomework2 5000 (-1999) (-999) 0
    , bad  "Deadline: 5000; TxValidRange (4999, 5000)" $ testHomework2 5000 (-1000) (-999) 0
    , bad  "Deadline: 5000; TxValidRange (4999, 5999)" $ testHomework2 5000 (-1000) 0 0
    ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000) cfg

testHomework2 :: POSIXTime -> POSIXTime -> POSIXTime -> Slot -> Run ()
testHomework2 dat curMinT curMaxT wSlot = do
  users <- setupUsers
  let [u1, u2, _u3] = users
      val = adaValue 100

  checkBalance (gives u1 val $ script2 u2) $ do
    sp <- spend u1 val
    submitTx u1 $ misteryTx2 u2 dat sp val
  waitNSlots wSlot
  utxos <- utxoAt $ script2 u2
  let [(vestRef, vestOut)] = utxos
  checkBalance (gives (script2 u2) (txOutValue vestOut) u2) $ do
    range <- currentTimeInterval curMinT curMaxT
    tx <- validateIn range $ claimingTx2 u2 dat vestRef (txOutValue vestOut)
    submitTx u2 tx

misteryTx2 :: PubKeyHash -> POSIXTime -> UserSpend -> Value -> Tx
misteryTx2 pkh dat usp val =
  mconcat
    [ userSpend usp
    , payToScript (script2 pkh) (HashDatum dat) val
    ]

claimingTx2 :: PubKeyHash -> POSIXTime -> TxOutRef -> Value -> Tx
claimingTx2 receiver dat vestRef vestVal =
  mconcat
    [ spendScript (script2 receiver) vestRef () dat
    , payToKey receiver vestVal
    ]
