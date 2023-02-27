{-# LANGUAGE NumericUnderscores #-}

module Main where

import           Control.Monad        (replicateM, unless)
import           Plutus.Model
import           Plutus.V2.Ledger.Api
import           Prelude
import           Test.Tasty
import qualified Homework1            as H1
import qualified Homework2            as H2

type Homework1Script = TypedValidator () (Bool, Bool)

script1 :: Homework1Script
script1 = TypedValidator $ toV2 H1.validator

type Homework2Script = TypedValidator () H2.MyRedeemer

script2 :: Homework2Script
script2 = TypedValidator $ toV2 H2.validator

setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 3 $ newUser $ ada (Lovelace 1000)

main :: IO ()
main = do
  defaultMain $ do
    testGroup "Homework tests"
      [ 
        homework1 defaultBabbage
      , homework2 defaultBabbage
      ]

homework1 :: MockConfig -> TestTree
homework1 cfg = do
  testGroup
    "Testing Homework1"
    [ good "Case: (True, True)" $ giveGift (True, True)
    , bad "Case: (True, False)" $ giveGift (True, False)
    , bad "Case: (False, True)" $ giveGift (False, True)
    , bad "Case: (False, False)" $ giveGift (False, False)
    ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000) cfg

giveGift :: (Bool, Bool) -> Run ()
giveGift redeemer = do
  users <- setupUsers
  let [u1, u2, _u3] = users
      val = adaValue 100
  checkBalance (gives u1 val script1) $ do
    sp <- spend u1 val
    submitTx u1 $ giveTx sp val

  utxos <- utxoAt script1
  let [(giftRef, giftOut)] = utxos
  checkBalance (gives script1 (txOutValue giftOut) u2) $ do
    submitTx u2 $ takeTx u2 redeemer giftRef (txOutValue giftOut)
  
  vals <- mapM valueAt users
  let [v1, v2, _] = vals
  unless (v1 == adaValue 900 && v2 == adaValue 1100) $
    logError "Final balances are incorrect"

giveTx :: UserSpend -> Value -> Tx
giveTx usp val = 
  mconcat
    [ userSpend usp
    , payToScript script1 (HashDatum ()) val
    ]

takeTx :: PubKeyHash -> (Bool, Bool) -> TxOutRef -> Value -> Tx
takeTx pkh redeemer giftRef giftVal =
  mconcat
    [ spendScript script1 giftRef redeemer ()
    , payToKey pkh giftVal
    ]


homework2 :: MockConfig -> TestTree
homework2 cfg = do
  testGroup
    "Testing Homework2"
    [ bad "Case: MyRedeemer True True" $ giveGift' (H2.MyRedeemer True True)
    , good "Case: MyRedeemer True False" $ giveGift' (H2.MyRedeemer True False)
    , good "Case: MyRedeemer False True" $ giveGift' (H2.MyRedeemer False True)
    , bad "Case: MyRedeemer False False" $ giveGift' (H2.MyRedeemer False False)
    ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000) cfg

giveGift' :: H2.MyRedeemer -> Run ()
giveGift' redeemer = do
  users <- setupUsers
  let [u1, u2, _u3] = users
      val = adaValue 100
  checkBalance (gives u1 val script2) $ do
    sp <- spend u1 val
    submitTx u1 $ giveTx' sp val

  utxos <- utxoAt script2
  let [(giftRef, giftOut)] = utxos
  checkBalance (gives script2 (txOutValue giftOut) u2) $ do
    submitTx u2 $ takeTx' u2 redeemer giftRef (txOutValue giftOut)
  
  vals <- mapM valueAt users
  let [v1, v2, _] = vals
  unless (v1 == adaValue 900 && v2 == adaValue 1100) $
    logError "Final balances are incorrect"

giveTx' :: UserSpend -> Value -> Tx
giveTx' usp val = 
  mconcat
    [ userSpend usp
    , payToScript script2 (HashDatum ()) val
    ]

takeTx' :: PubKeyHash -> H2.MyRedeemer -> TxOutRef -> Value -> Tx
takeTx' pkh redeemer giftRef giftVal =
  mconcat
    [ spendScript script2 giftRef redeemer ()
    , payToKey pkh giftVal
    ]
