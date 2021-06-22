{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Spec.Model
    ( tests
    , test
    , TSModel (..)
    )  where

import           Control.Lens                       hiding (elements)
import           Control.Monad                      (void, when)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (isJust, isNothing)
import           Data.Monoid                        (Last (..))
import           Data.String                        (IsString (..))
import           Data.Text                          (Text)
import           Plutus.Contract.Test
import           Plutus.Contract.Test.ContractModel
import           Plutus.Trace.Emulator
import           Ledger                             hiding (singleton)
import           Ledger.Ada                         as Ada
import           Ledger.Value
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Week08.TokenSale                   (TokenSale (..), TSStartSchema', TSUseSchema, startEndpoint', useEndpoints, nftName)

data TSState = TSState
    { _tssPrice    :: !Integer
    , _tssLovelace :: !Integer
    , _tssToken    :: !Integer
    } deriving Show

makeLenses ''TSState

newtype TSModel = TSModel {_tsModel :: Map Wallet TSState}
    deriving Show

makeLenses ''TSModel

tests :: TestTree
tests = testProperty "token sale model" prop_TS

instance ContractModel TSModel where

    data Action TSModel =
              Start Wallet
            | SetPrice Wallet Wallet Integer
            | AddTokens Wallet Wallet Integer
            | Withdraw Wallet Wallet Integer Integer
            | BuyTokens Wallet Wallet Integer
        deriving (Show, Eq)

    data ContractInstanceKey TSModel w s e where
        StartKey :: Wallet           -> ContractInstanceKey TSModel (Last TokenSale) TSStartSchema' Text
        UseKey   :: Wallet -> Wallet -> ContractInstanceKey TSModel ()               TSUseSchema    Text

    instanceTag key _ = fromString $ "instance tag for: " ++ show key

    arbitraryAction _ = oneof $
        (Start <$> genWallet) :
        [ SetPrice  <$> genWallet <*> genWallet <*> genNonNeg ]               ++
        [ AddTokens <$> genWallet <*> genWallet <*> genNonNeg ]               ++
        [ BuyTokens <$> genWallet <*> genWallet <*> genNonNeg ]               ++
        [ Withdraw  <$> genWallet <*> genWallet <*> genNonNeg <*> genNonNeg ]

    initialState = TSModel Map.empty

    nextState (Start w) = do
        withdraw w $ nfts Map.! w
        (tsModel . at w) $= Just (TSState 0 0 0)
        wait 1

    nextState (SetPrice v w p) = do
        when (v == w) $
            (tsModel . ix v . tssPrice) $= p
        wait 1

    nextState (AddTokens v w n) = do
        started <- hasStarted v                                     -- has the token sale started?
        when (n > 0 && started) $ do
            bc <- askModelState $ view $ balanceChange w
            let token = tokens Map.! v
            when (tokenAmt + assetClassValueOf bc token >= n) $ do  -- does the wallet have the tokens to give?
                withdraw w $ assetClassValue token n
                (tsModel . ix v . tssToken) $~ (+ n)
        wait 1

    nextState (BuyTokens v w n) = do
        when (n > 0) $ do
            m <- getTSState v
            case m of
                Just t
                    | t ^. tssToken >= n -> do
                        let p = t ^. tssPrice
                            l = p * n
                        withdraw w $ lovelaceValueOf l
                        deposit w $ assetClassValue (tokens Map.! v) n
                        (tsModel . ix v . tssLovelace) $~ (+ l)
                        (tsModel . ix v . tssToken)    $~ (+ (- n))
                _ -> return ()
        wait 1

    nextState (Withdraw v w n l) = do
        when (v == w) $ do
            m <- getTSState v
            case m of
                Just t
                    | t ^. tssToken >= n && t ^. tssLovelace >= l -> do
                        deposit w $ lovelaceValueOf l <> assetClassValue (tokens Map.! w) n
                        (tsModel . ix v . tssLovelace) $~ (+ (- l))
                        (tsModel . ix v . tssToken) $~ (+ (- n))
                _ -> return ()
        wait 1

    perform h _ cmd = case cmd of
        (Start w)          -> callEndpoint @"start"      (h $ StartKey w) (nftCurrencies Map.! w, tokenCurrencies Map.! w, tokenNames Map.! w) >> delay 1
        (SetPrice v w p)   -> callEndpoint @"set price"  (h $ UseKey v w) p                                                                    >> delay 1
        (AddTokens v w n)  -> callEndpoint @"add tokens" (h $ UseKey v w) n                                                                    >> delay 1
        (BuyTokens v w n)  -> callEndpoint @"buy tokens" (h $ UseKey v w) n                                                                    >> delay 1
        (Withdraw v w n l) -> callEndpoint @"withdraw"   (h $ UseKey v w) (n, l)                                                               >> delay 1

    precondition s (Start w)          = isNothing $ getTSState' s w
    precondition s (SetPrice v _ _)   = isJust    $ getTSState' s v
    precondition s (AddTokens v _ _)  = isJust    $ getTSState' s v
    precondition s (BuyTokens v _ _)  = isJust    $ getTSState' s v
    precondition s (Withdraw v _ _ _) = isJust    $ getTSState' s v

deriving instance Eq (ContractInstanceKey TSModel w s e)
deriving instance Show (ContractInstanceKey TSModel w s e)

getTSState' :: ModelState TSModel -> Wallet -> Maybe TSState
getTSState' s v = s ^. contractState . tsModel . at v

getTSState :: Wallet -> Spec TSModel (Maybe TSState)
getTSState v = do
    s <- getModelState
    return $ getTSState' s v

hasStarted :: Wallet -> Spec TSModel Bool
hasStarted v = isJust <$> getTSState v

w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2

wallets :: [Wallet]
wallets = [w1, w2]

tokenCurrencies, nftCurrencies :: Map Wallet CurrencySymbol
tokenCurrencies = Map.fromList $ zip wallets ["aa", "bb"]
nftCurrencies   = Map.fromList $ zip wallets ["01", "02"]

tokenNames :: Map Wallet TokenName
tokenNames = Map.fromList $ zip wallets ["A", "B"]

tokens :: Map Wallet AssetClass
tokens = Map.fromList [(w, AssetClass (tokenCurrencies Map.! w, tokenNames Map.! w)) | w <- wallets]

nftAssets :: Map Wallet AssetClass
nftAssets = Map.fromList [(w, AssetClass (nftCurrencies Map.! w, nftName)) | w <- wallets]

nfts :: Map Wallet Value
nfts = Map.fromList [(w, assetClassValue (nftAssets Map.! w) 1) | w <- wallets]

tss :: Map Wallet TokenSale
tss = Map.fromList
    [ (w, TokenSale { tsSeller =  pubKeyHash $ walletPubKey w
                    , tsToken  =  tokens Map.! w
                    , tsNFT    =  nftAssets Map.! w
                    })
    | w <- wallets
    ]

delay :: Int -> EmulatorTrace ()
delay = void . waitNSlots . fromIntegral

instanceSpec :: [ContractInstanceSpec TSModel]
instanceSpec =
    [ContractInstanceSpec (StartKey w) w startEndpoint' | w <- wallets] ++
    [ContractInstanceSpec (UseKey v w) w $ useEndpoints $ tss Map.! v | v <- wallets, w <- wallets]

genWallet :: Gen Wallet
genWallet = elements wallets

genNonNeg :: Gen Integer
genNonNeg = getNonNegative <$> arbitrary

tokenAmt :: Integer
tokenAmt = 1_000

prop_TS :: Actions TSModel -> Property
prop_TS = withMaxSuccess 100 . propRunActionsWithOptions
    (defaultCheckOptions & emulatorConfig .~ EmulatorConfig (Left d))
    instanceSpec
    (const $ pure True)
  where
    d :: InitialDistribution
    d = Map.fromList $ [ ( w
                         , lovelaceValueOf 1000_000_000 <>
                           (nfts Map.! w)               <>
                           mconcat [assetClassValue t tokenAmt | t <- Map.elems tokens])
                       | w <- wallets
                       ]

test :: IO ()
test = quickCheck prop_TS
