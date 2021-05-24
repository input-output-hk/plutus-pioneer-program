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

module Week08.TestTokenSale where

import           Control.Lens                       hiding (elements)
import           Control.Monad                      (void, when)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (isJust, isNothing)
import           Data.Monoid                        (Last (..))
import           Data.Text                          (Text)
import           Plutus.Contract.Test
import           Plutus.Contract.Test.ContractModel
import           Plutus.Trace.Emulator
import           Ledger                             hiding (singleton)
import           Ledger.Ada                         as Ada
import           Ledger.Value
import           Test.QuickCheck

import           Week08.TokenSale

data TSState = TSState
    { _tssPrice    :: !Integer
    , _tssLovelace :: !Integer
    , _tssToken    :: !Integer
    } deriving Show

makeLenses ''TSState

newtype TSModel = TSModel {_tsModel :: Map Wallet TSState}
    deriving Show

makeLenses ''TSModel

instance ContractModel TSModel where

    data Action TSModel = Start Wallet | TSAction Wallet Wallet TSRedeemer
        deriving (Show, Eq)

    data ContractInstanceKey TSModel w s e where
        StartKey :: Wallet           -> ContractInstanceKey TSModel (Last TokenSale) TSStartSchema' Text
        UseKey   :: Wallet -> Wallet -> ContractInstanceKey TSModel () TSUseSchema Text

    arbitraryAction _ = oneof $
        (Start <$> genSeller) :
        [ (\v w p   -> TSAction v w $ SetPrice p)   <$> genSeller <*> genUser <*> arbitrary ] ++
        [ (\v w n   -> TSAction v w $ AddTokens n)  <$> genSeller <*> genUser <*> arbitrary ] ++
        [ (\v w n   -> TSAction v w $ BuyTokens n)  <$> genSeller <*> genUser <*> arbitrary ] ++
        [ (\v w n l -> TSAction v w $ Withdraw n l) <$> genSeller <*> genUser <*> arbitrary <*> arbitrary ]

    initialState = TSModel Map.empty

    nextState (Start w) = do
        withdraw w $ nfts Map.! w
        (tsModel . at w) $= Just (TSState 0 0 0)
        wait 1

    nextState (TSAction v w (SetPrice p)) = when (v == w) $ do
        (tsModel . ix v . tssPrice) $= p
        wait 1

    nextState (TSAction v w (AddTokens n)) = do
        started <- hasStarted v                          -- has the token sale started?
        when (n > 0 && started) $ do
            bc <- askModelState $ view $ balanceChange w
            let token = tokens Map.! v
            when (assetClassValueOf bc token >= n) $ do  -- does the wallet have the tokens to give?
                withdraw w $ assetClassValue token n
                (tsModel . ix v . tssToken) $~ (+ n)
        wait 1

    nextState (TSAction v w (BuyTokens n)) = do
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
                        (tsModel . ix v . tssToken) $~ (+ (- n))
                _ -> return ()
        wait 1

    nextState (TSAction v w (Withdraw n l)) = when (v == w) $ do
        withdraw w $ lovelaceValueOf l <> assetClassValue (tokens Map.! v) n
        (tsModel . ix v . tssLovelace) $~ (+ (- l))
        (tsModel . ix v . tssToken) $~ (+ (- n))
        wait 1

    perform h _ cmd = case cmd of
        (Start w)                     -> callEndpoint @"start"      (h $ StartKey w) (css Map.! w, tokenCurrency, tokenNames Map.! w) >> delay 1
        (TSAction v w (SetPrice p))   -> callEndpoint @"set price"  (h $ UseKey v w) p                                                >> delay 1
        (TSAction v w (AddTokens n))  -> callEndpoint @"add tokens" (h $ UseKey v w) n                                                >> delay 1
        (TSAction v w (BuyTokens n))  -> callEndpoint @"buy tokens" (h $ UseKey v w) n                                                >> delay 1
        (TSAction v w (Withdraw n l)) -> callEndpoint @"withdraw"   (h $ UseKey v w) (n, l)                                           >> delay 1

    precondition s (Start w) = isNothing $ getTSState' s w
    precondition _ _         = True

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

w1, w2, w3, w4 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3
w4 = Wallet 4

tokenCurrency :: CurrencySymbol
tokenCurrency = "ff"

tokenNames :: Map Wallet TokenName
tokenNames = Map.fromList [(w1, "A"), (w2, "B")]

tokens :: Map Wallet AssetClass
tokens = (\tn -> AssetClass (tokenCurrency, tn)) <$> tokenNames

wallets :: [Wallet]
wallets = [w1, w2, w3, w4]

css :: Map Wallet CurrencySymbol
css = Map.fromList [(w1, "01"), (w2, "02")]

nfts :: Map Wallet Value
nfts = (\cs -> assetClassValue (AssetClass (cs, nftName)) 1) <$> css

tss :: Map Wallet TokenSale
tss = Map.fromList
    [ (w, TokenSale (pubKeyHash $ walletPubKey w) (tokens Map.! w) $ AssetClass (css Map.! w, nftName))
    | w <- [w1, w2]
    ]

delay :: Int -> EmulatorTrace ()
delay = void . waitNSlots . fromIntegral

instanceSpec :: [ContractInstanceSpec TSModel]
instanceSpec =
    [ContractInstanceSpec (StartKey w) w $ startTS'' | w <- [w1, w2]] ++
    [ContractInstanceSpec (UseKey v w) w $ useTS $ tss Map.! v | v <- [w1, w2], w <- [w3, w4]]

genSeller, genUser :: Gen Wallet
genSeller = elements [w1, w2]
genUser   = elements [w3, w4]

prop_TS :: Actions TSModel -> Property
prop_TS = propRunActionsWithOptions
    (defaultCheckOptions & emulatorConfig .~ EmulatorConfig (Left d))
    instanceSpec
    (const $ pure True)
  where
    d :: InitialDistribution
    d = Map.fromList $ [ ( w
                         , lovelaceValueOf 1000_000_000 <>
                           (nfts Map.! w)               <>
                           mconcat [assetClassValue t 1000 | t <- Map.elems tokens])
                       | w <- [w1, w2]
                       ] ++
                       [(w, lovelaceValueOf 1000_000_000) | w <- [w3, w4]]


test :: IO ()
test = quickCheck prop_TS
