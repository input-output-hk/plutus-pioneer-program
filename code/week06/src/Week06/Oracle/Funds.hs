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

module Week06.Oracle.Funds
    ( ownFunds
    , ownFunds'
    ) where

import           Control.Monad    hiding (fmap)
import qualified Data.Map         as Map
import           Data.Monoid      (Last (..))
import           Data.Text        (Text)
import           Plutus.Contract  as Contract hiding (when)
import           PlutusTx.Prelude hiding ((<$>))
import           Prelude          ((<$>))
import           Ledger           hiding (singleton)
import           Ledger.Value     as Value

ownFunds :: HasBlockchainActions s => Contract w s Text Value
ownFunds = do
    pk    <- ownPubKey
    utxos <- utxoAt $ pubKeyAddress pk
    let v = mconcat $ Map.elems $ txOutValue . txOutTxOut <$> utxos
    logInfo @String $ "own funds: " ++ show (Value.flattenValue v)
    return v

ownFunds' :: Contract (Last Value) BlockchainActions Text ()
ownFunds' = do
    handleError logError $ ownFunds >>= tell . Last . Just
    void $ Contract.waitNSlots 1
    ownFunds'
