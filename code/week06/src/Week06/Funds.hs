{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week06.Funds
    ( funds
    , funds'
    ) where

import           Control.Monad    hiding (fmap)
import qualified Data.Map         as Map
import           Data.Monoid      (Last (..))
import           Data.Text        (Text)
import           Plutus.Contract  as Contract
import           PlutusTx.Prelude hiding ((<$>))
import           Prelude          (Show (..), String, (<$>))
import           Ledger           hiding (singleton)
import           Ledger.Value     as Value

funds :: Address -> Contract w s Text Value
funds addr = do
    utxos <- utxosAt addr
    let v = mconcat $ Map.elems $ _ciTxOutValue <$> utxos
    logInfo @String $ "funds at " ++ show addr ++ ": " ++ show (Value.flattenValue v)
    return v

funds' :: Address -> Contract (Last Value) Empty Text a
funds' addr = do
    handleError logError $ funds addr >>= tell . Last . Just
    void $ Contract.waitNSlots 1
    funds' addr
