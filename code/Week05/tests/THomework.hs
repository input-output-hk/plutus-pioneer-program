{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Monad                 (replicateM)
import           Plutus.Model
import           Plutus.V1.Ledger.Interval (interval)
import           Plutus.V2.Ledger.Api
import           Prelude
import           Test.Tasty
import qualified Homework1                     as H1
import qualified Homework2                     as H2

type HomeworkScript = TypedPolicy ()

script1 :: PubKeyHash -> POSIXTime -> HomeworkScript
script1 pkh time = TypedPolicy $ toV2 $ H1.deadlinePolicy pkh time

setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 3 $ newUser $ ada (Lovelace 1000)

main :: IO ()
main = do
  defaultMain $ do
    testGroup "Homework tests"
      [ homework1 defaultBabbage
      , homework2 defaultBabbage
      ]
range :: (POSIXTime, POSIXTime) -> POSIXTimeRange
range (start, end) = interval start end

data Case = Case
  { parameterUser :: Int
  , signerUser    :: Int
  , deadline :: POSIXTime
  , gotoTime :: POSIXTime
  , rStart :: POSIXTime
  , rEnd   :: POSIXTime }

goodCases :: [Case]
goodCases = [ Case 0 0 10000 9000 9000 10000
            , Case 1 1 10000 9999 9000 9999
            , Case 2 2 10000 10999 9999 10001
            , Case 0 0 10000 10999 9999 10999
            , Case 1 1 10000 10999 9999 10500
            ]

wrongSigner :: [Case]
wrongSigner = [ Case 0 1 10000 9000 9000 10000
              , Case 0 2 10000 9000 9000 10000
              , Case 1 2 10000 9000 9000 10000
              , Case 1 2 10000 9000 9000 10000
              , Case 2 0 10000 9000 9000 10000
              , Case 2 1 10000 9000 9000 10000
              ]

wrongRange :: [Case]
wrongRange = [ Case 0 0 10000 10000 10000 11000
             , Case 1 1 10000 10000 9000 12000
             , Case 2 2 10000 11000 11000 11000
             , Case 0 0 10000 12000 11000 13000
             ]

homework1 :: MockConfig -> TestTree
homework1 cfg = do
  testGroup
    "Testing Homework1" $
    (map
     toGoodTest
     goodCases) ++
    (map
     toBadTest
     wrongSigner) ++
    (map
     toBadTest
     wrongRange)
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000) cfg
    toGoodTest :: Case -> TestTree
    toGoodTest (Case {..}) = good ("Deadline: " ++ show (getPOSIXTime deadline)
           ++ "; TxValidRange: (" ++ show (getPOSIXTime rStart) ++ "," ++ show (getPOSIXTime rEnd) ++ ")"
           ++ "; Signer User: " ++ show signerUser
           ++ "; Go to time: " ++ show (getPOSIXTime gotoTime)
           ++ "; Beneficiary: " ++ show parameterUser) $
        testHomework1 parameterUser signerUser deadline gotoTime (range (rStart, rEnd))
    toBadTest :: Case -> TestTree
    toBadTest (Case {..}) = bad ("Deadline: " ++ show (getPOSIXTime deadline)
           ++ "; TxValidRange: (" ++ show (getPOSIXTime rStart) ++ "," ++ show (getPOSIXTime rEnd) ++ ")"
           ++ "; Signer User: " ++ show signerUser
           ++ "; Go to time: " ++ show (getPOSIXTime gotoTime)
           ++ "; Beneficiary: " ++ show parameterUser) $
        testHomework1 parameterUser signerUser deadline gotoTime (range (rStart, rEnd))

testHomework1 :: Int -> Int -> POSIXTime -> POSIXTime -> POSIXTimeRange -> Run ()
testHomework1 ben s date wTime range' = do
  users <- setupUsers
  let signer = users !! s
      beneficiary = users !! ben
      typedPolicy = script1 beneficiary date
      mintingValue = singleton (scriptCurrencySymbol typedPolicy) "1234" 1234
  waitUntil wTime
  checkBalance (owns beneficiary mintingValue) $ do
    tx <- validateIn range' $ mintingTx typedPolicy mintingValue beneficiary
    submitTx signer tx

mintingTx :: (TypedPolicy ()) -> Value -> PubKeyHash -> Tx
mintingTx tp val pkh =
  mconcat
    [ mintValue tp () $ singleton (scriptCurrencySymbol tp) "1234" 1234
    , payToKey pkh val
    ]


script2 :: TxOutRef -> TokenName -> HomeworkScript
script2 txOutRef tn = TypedPolicy $ toV2 $ H2.nftPolicy txOutRef tn

homework2 :: MockConfig -> TestTree
homework2 cfg = do
  testGroup
    "Testing Homework2" $
     [ good "User 0 signs"              $ changeSigner 0
     , bad  "User 1 signs"              $ changeSigner 1
     , bad  "User 2 signs"              $ changeSigner 2
     , good "Amount = 1"                $ changeAmount 1
     , bad  "Amount = 2"                $ changeAmount 2
     , bad  "Amount = 1000"             $ changeAmount 1000
     , bad  "Amount = -1"               $ changeAmount (-1)
     , good "TokenName = \"\""          $ changeTokenName ""
     , bad  "TokenName = \"TokenName\"" $ changeTokenName "TokenName"
     , bad  "TokenName = \"empty\""     $ changeTokenName "empty"
     , bad  "TokenName = \"foo\""       $ changeTokenName "foo"
     , good "First user UTxO"           $ changeCurrencySymbol 0
     , bad  "Second user UTxO"          $ changeCurrencySymbol 1
     , bad  "Third user UTxO"           $ changeCurrencySymbol 2
     , bad  "Consume another UTxO"      $ comsumeAnotherUTxO
     , bad  "Two NFTs"                  $ twoNFTs
     ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000) cfg

changeSigner :: Int -> Run ()
changeSigner sig = do
  users <- setupUsers
  let beneficiary = users !! 0
      signer = users !! sig

  utxos <- utxoAt beneficiary
  let [(ref, out)] = utxos
      typedPolicy = script2 ref ""
      mintingValue = singleton (scriptCurrencySymbol typedPolicy) "" 1
      valueInOutput = txOutValue out
  checkBalance (owns beneficiary mintingValue) $ do
    submitTx signer $ mintingNFTTx ref typedPolicy mintingValue valueInOutput signer

changeAmount :: Integer -> Run ()
changeAmount amt = do
  users <- setupUsers
  let beneficiary = users !! 0

  utxos <- utxoAt beneficiary
  let [(ref, out)] = utxos
      typedPolicy = script2 ref ""
      mintingValue = singleton (scriptCurrencySymbol typedPolicy) "" amt
      valueInOutput = txOutValue out
  checkBalance (owns beneficiary mintingValue) $ do
    submitTx beneficiary $ mintingNFTTx ref typedPolicy mintingValue valueInOutput beneficiary

changeTokenName :: TokenName -> Run ()
changeTokenName tn = do
  users <- setupUsers
  let beneficiary = users !! 0

  utxos <- utxoAt beneficiary
  let [(ref, out)] = utxos
      typedPolicy = script2 ref ""
      mintingValue = singleton (scriptCurrencySymbol typedPolicy) tn 1
      valueInOutput = txOutValue out
  checkBalance (owns beneficiary mintingValue) $ do
    submitTx beneficiary $ mintingNFTTx ref typedPolicy mintingValue valueInOutput beneficiary

changeCurrencySymbol :: Int -> Run ()
changeCurrencySymbol u = do
  users <- setupUsers
  let beneficiary = users !! 0
      user     = users !! u

  utxos <- utxoAt beneficiary
  wrongUtxos <- utxoAt user
  let [(ref, out)] = utxos
      [(wRef, wOut)] = wrongUtxos
      typedPolicy = script2 ref ""
      wrongTypedPolicy = script2 wRef ""
      wrongMintingValue = singleton (scriptCurrencySymbol wrongTypedPolicy) "" 1
      valueInOutput = txOutValue out
  checkBalance (owns beneficiary wrongMintingValue) $ do
    submitTx beneficiary $ mintingNFTTx ref typedPolicy wrongMintingValue valueInOutput beneficiary

comsumeAnotherUTxO :: Run ()
comsumeAnotherUTxO = do
  user <- newUser $ ada $ Lovelace 1000
  utxos <- utxoAt user
  let [(ref, out)] = utxos
  submitTx user $ splitUTxOTx user ref
  utxos' <- utxoAt user
  case utxos' of
    [(ref',out'),(wRef,wOut)] -> do
      let typedPolicy = script2 ref' ""
          mintingValue = singleton (scriptCurrencySymbol typedPolicy) "" 1
          valueInOutput = txOutValue out'
      submitTx user $ mintingNFTTx wRef typedPolicy mintingValue valueInOutput user
    _                         -> logError "Failed to split utxo"

twoNFTs :: Run ()
twoNFTs = do
  user <- newUser $ ada $ Lovelace 1000
  utxos <- utxoAt user
  let [(ref, out)] = utxos
  submitTx user $ splitUTxOTx user ref
  utxos' <- utxoAt user
  case utxos' of
    [(ref1,out1),(ref2,out2)] -> do
      let typedPolicy1 = script2 ref1 ""
          typedPolicy2 = script2 ref2 ""
          mintingValue1 = singleton (scriptCurrencySymbol typedPolicy1) "" 1
          mintingValue2 = singleton (scriptCurrencySymbol typedPolicy2) "" 1
          valueInOutput1 = txOutValue out1
          valueInOutput2 = txOutValue out2
      submitTx user $ mintingNFTTx ref1 typedPolicy1 mintingValue1 valueInOutput1 user
                   <> mintingNFTTx ref2 typedPolicy2 mintingValue2 valueInOutput2 user
    _                         -> logError "Failed to split utxo"

mintingNFTTx :: TxOutRef -> (TypedPolicy ()) -> Value -> Value -> PubKeyHash -> Tx
mintingNFTTx txOutRef tp val vInOutput pkh =
  mconcat
    [ mintValue tp () val
    , payToKey pkh $ val <> vInOutput
    , spendPubKey txOutRef
    ]

splitUTxOTx :: PubKeyHash -> TxOutRef -> Tx
splitUTxOTx pkh txOutRef= mconcat
                [ payToKey pkh (ada (Lovelace 500))
                , payToKey pkh (ada (Lovelace 500))
                , spendPubKey txOutRef
                ]