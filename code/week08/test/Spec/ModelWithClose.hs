{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Spec.ModelWithClose
    ( tests
    , test
    , TSModel (..)
    )  where

import           Control.Lens                                 hiding (elements)
import           Control.Monad                                (forM_, void, when)
import           Data.Map                                     (Map)
import qualified Data.Map                                     as Map
import           Data.Maybe                                   (isJust, isNothing)
import           Data.Monoid                                  (Last (..))
import           Data.String                                  (IsString (..))
import           Data.Text                                    (Text)
import           Plutus.Contract
import           Plutus.Contract.Test
import           Plutus.Contract.Test.ContractModel           as Test
import           Plutus.Contract.Test.ContractModel.Symbolics
import           Plutus.Trace.Emulator                        as Trace
import           Ledger                                       hiding (singleton)
import           Ledger.Ada                                   as Ada
import           Ledger.Value
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Week08.TokenSaleWithClose                    (TokenSale (..), TSStartSchema, TSUseSchema, startEndpoint, useEndpoints')

type TSUseSchema' = TSUseSchema .\/ Endpoint "init" TokenSale .\/ Endpoint "kill" ()

useEndpoints'' :: Contract () TSUseSchema' Text ()
useEndpoints'' = awaitPromise $ endpoint @"init" go
  where
    go :: TokenSale -> Contract () TSUseSchema' Text ()
    go ts = awaitPromise $ (useEndpoints' ts `promiseBind` \() -> go ts) `select` (endpoint @"kill" $ \() -> useEndpoints'')

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
tests = testProperty "token sale (with close) model" prop_TS

instance ContractModel TSModel where

    data Action TSModel =
              Start Wallet
            | SetPrice Wallet Wallet Integer
            | AddTokens Wallet Wallet Integer
            | Withdraw Wallet Wallet Integer Integer
            | BuyTokens Wallet Wallet Integer
            | Close Wallet Wallet
        deriving (Show, Eq)

    data ContractInstanceKey TSModel w s e p where
        StartKey :: Wallet           -> ContractInstanceKey TSModel (Last TokenSale) TSStartSchema Text ()
        UseKey   :: Wallet -> Wallet -> ContractInstanceKey TSModel ()               TSUseSchema'  Text ()

    instanceWallet :: ContractInstanceKey TSModel w s e p -> Wallet
    instanceWallet (StartKey w) = w
    instanceWallet (UseKey _ w) = w

    instanceTag :: SchemaConstraints w s e => ContractInstanceKey TSModel w s e p -> ContractInstanceTag
    instanceTag key = fromString $ "instance tag for: " ++ show key

    arbitraryAction :: ModelState TSModel -> Gen (Action TSModel)
    arbitraryAction _ = oneof
        [ Start     <$> genWallet
        , SetPrice  <$> genWallet <*> genWallet <*> genNonNeg
        , AddTokens <$> genWallet <*> genWallet <*> genNonNeg
        , BuyTokens <$> genWallet <*> genWallet <*> genNonNeg
        , Withdraw  <$> genWallet <*> genWallet <*> genNonNeg <*> genNonNeg
        , Close     <$> genWallet <*> genWallet
        ]

    initialState :: TSModel
    initialState = TSModel Map.empty

    initialInstances :: [StartContract TSModel]
    initialInstances =    [StartContract (StartKey v) () | v <- wallets]
                       ++ [StartContract (UseKey v w) () | v <- wallets, w <- wallets]

    precondition :: ModelState TSModel -> Action TSModel -> Bool
    precondition s (Start w)          = isNothing $ getTSState' s w
    precondition s (SetPrice v _ _)   = isJust    $ getTSState' s v
    precondition s (AddTokens v _ _)  = isJust    $ getTSState' s v
    precondition s (BuyTokens v _ _)  = isJust    $ getTSState' s v
    precondition s (Withdraw v _ _ _) = isJust    $ getTSState' s v
    precondition s (Close v _)        = isJust    $ getTSState' s v

    nextState :: Action TSModel -> Spec TSModel ()
    nextState (Start w) = do
        wait 3
        (tsModel . at w) $= Just (TSState 0 0 0)
        withdraw w $ Ada.toValue minAdaTxOut
    nextState (SetPrice v w p) = do
        wait 3
        when (v == w) $
            (tsModel . ix v . tssPrice) $= p
    nextState (AddTokens v w n) = do
        wait 3
        started <- hasStarted v                                     -- has the token sale started?
        when (n > 0 && started) $ do
            bc <- actualValPart <$> askModelState (view $ balanceChange w)
            let token = tokens Map.! v
            when (tokenAmt + assetClassValueOf bc token >= n) $ do  -- does the wallet have the tokens to give?
                withdraw w $ assetClassValue token n
                (tsModel . ix v . tssToken) $~ (+ n)
    nextState (BuyTokens v w n) = do
        wait 3
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
    nextState (Withdraw v w n l) = do
        wait 3
        when (v == w) $ do
            m <- getTSState v
            case m of
                Just t
                    | t ^. tssToken >= n && t ^. tssLovelace >= l -> do
                        deposit w $ lovelaceValueOf l <> assetClassValue (tokens Map.! w) n
                        (tsModel . ix v . tssLovelace) $~ (+ (- l))
                        (tsModel . ix v . tssToken) $~ (+ (- n))
                _ -> return ()
    nextState (Close v w) = do
        wait 4
        when (v == w) $ do
            m <- getTSState v
            case m of
                Just t -> do
                    deposit w $ lovelaceValueOf (t ^. tssLovelace)               <>
                                assetClassValue (tokens Map.! w) (t ^. tssToken) <>
                                Ada.toValue minAdaTxOut
                    (tsModel . at v) $= Nothing
                _ -> return ()


    startInstances :: ModelState TSModel -> Action TSModel -> [StartContract TSModel]
    startInstances _ _ = []

    instanceContract :: (SymToken -> AssetClass) -> ContractInstanceKey TSModel w s e p -> p -> Contract w s e ()
    instanceContract _ (StartKey _) () = startEndpoint
    instanceContract _ (UseKey _ _) () = useEndpoints''

    perform :: HandleFun TSModel -> (SymToken -> AssetClass) -> ModelState TSModel -> Action TSModel -> SpecificationEmulatorTrace ()
    perform h _ m (Start v)         = do
        let handle = h $ StartKey v
        withWait m $ callEndpoint @"start" handle (tokenCurrencies Map.! v, tokenNames Map.! v)
        Last mts <- observableState handle
        case mts of
            Nothing -> Trace.throwError $ GenericError $ "starting token sale for wallet " ++ show v ++ " failed"
            Just ts -> forM_ wallets $ \w ->
                callEndpoint @"init" (h $ UseKey v w) ts
    perform h _ m (SetPrice v w p)   = withWait m $ callEndpoint @"set price"  (h $ UseKey v w) p
    perform h _ m (AddTokens v w n)  = withWait m $ callEndpoint @"add tokens" (h $ UseKey v w) n
    perform h _ m (BuyTokens v w n)  = withWait m $ callEndpoint @"buy tokens" (h $ UseKey v w) n
    perform h _ m (Withdraw v w n l) = withWait m $ callEndpoint @"withdraw"   (h $ UseKey v w) (n, l)
    perform h _ m (Close v w)        = do
        withWait m $ callEndpoint @"close" (h $ UseKey v w) ()
        when (v == w) $ forM_ wallets $ \w' ->
            callEndpoint @"kill" (h $ UseKey v w') ()
        delay 1

withWait :: ModelState TSModel -> SpecificationEmulatorTrace () -> SpecificationEmulatorTrace ()
withWait m c = void $ c >> waitUntilSlot ((m ^. Test.currentSlot) + 3)

deriving instance Eq (ContractInstanceKey TSModel w s e p)
deriving instance Show (ContractInstanceKey TSModel w s e p)

getTSState' :: ModelState TSModel -> Wallet -> Maybe TSState
getTSState' s v = s ^. contractState . tsModel . at v

getTSState :: Wallet -> Spec TSModel (Maybe TSState)
getTSState v = do
    s <- getModelState
    return $ getTSState' s v

hasStarted :: Wallet -> Spec TSModel Bool
hasStarted v = isJust <$> getTSState v

wallets :: [Wallet]
wallets = [w1, w2]

tokenCurrencies :: Map Wallet CurrencySymbol
tokenCurrencies = Map.fromList $ zip wallets ["aa", "bb"]

tokenNames :: Map Wallet TokenName
tokenNames = Map.fromList $ zip wallets ["A", "B"]

tokens :: Map Wallet AssetClass
tokens = Map.fromList [(w, AssetClass (tokenCurrencies Map.! w, tokenNames Map.! w)) | w <- wallets]

genWallet :: Gen Wallet
genWallet = elements wallets

genNonNeg :: Gen Integer
genNonNeg = getNonNegative <$> arbitrary

tokenAmt :: Integer
tokenAmt = 1_000

prop_TS :: Actions TSModel -> Property
prop_TS = withMaxSuccess 100 . propRunActionsWithOptions
    (defaultCheckOptions & emulatorConfig . initialChainState .~ Left d)
    defaultCoverageOptions
    (const $ pure True)
  where

    d :: InitialDistribution
    d = Map.fromList $ [ ( w
                         , lovelaceValueOf 1_000_000_000 <>
                           mconcat [assetClassValue t tokenAmt | t <- Map.elems tokens])
                       | w <- wallets
                       ]

test :: IO ()
test = quickCheck prop_TS
