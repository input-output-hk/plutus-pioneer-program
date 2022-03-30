{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week10.Staking
    ( stakeValidator
    ) where

import           Ledger
import           Ledger.Typed.Scripts        as Scripts
import           Plutus.V1.Ledger.Ada        (Ada (..), fromValue)
import           Plutus.V1.Ledger.Credential (StakingCredential)
import qualified PlutusTx
import           PlutusTx.Prelude

{-# INLINABLE mkStakingValidator #-}
mkStakingValidator :: Address -> () -> ScriptContext -> Bool
mkStakingValidator addr () ctx = case scriptContextPurpose ctx of
    Certifying _   -> True
    Rewarding cred -> traceIfFalse "insufficient reward sharing" $ 2 * paidToAddress >= amount cred
    _              -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    amount :: StakingCredential -> Integer
    amount cred = go $ txInfoWdrl info
      where
        go :: [(StakingCredential, Integer)] -> Integer
        go [] = traceError "withdrawal not found"
        go ((cred', amt) : xs)
            | cred' == cred = amt
            | otherwise     = go xs

    paidToAddress :: Integer
    paidToAddress = foldl f 0 $ txInfoOutputs info
      where
        f :: Integer -> TxOut -> Integer
        f n o
            | txOutAddress o == addr = n + getLovelace (fromValue $ txOutValue o)
            | otherwise              = n

stakeValidator :: Address -> StakeValidator
stakeValidator addr = mkStakeValidatorScript $
    $$(PlutusTx.compile [|| wrapStakeValidator . mkStakingValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode addr
