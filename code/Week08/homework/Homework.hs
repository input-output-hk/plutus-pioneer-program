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

module Homework
    ( stakeValidator'
    , saveStakeValidator'
    ) where

import           Plutus.V2.Ledger.Api (Address, BuiltinData, PubKeyHash,
                                       ScriptContext, StakeValidator,
                                       mkStakeValidatorScript)
import qualified PlutusTx
import           PlutusTx.Prelude     (Bool (..), ($), (.))
import           Prelude              (IO, String, undefined)
import           Utilities            (wrapStakeValidator)

-- | A staking validator with two parameters, a pubkey hash and an address. The validator
--   should work as follows:
--   1.) The given pubkey hash needs to sign all transactions involving this validator.
--   2.) The given address needs to receive at least half of all withdrawn rewards.
{-# INLINABLE mkStakeValidator' #-}
mkStakeValidator' :: PubKeyHash -> Address -> () -> ScriptContext -> Bool
mkStakeValidator' _pkh _addr () _ctx = undefined

{-# INLINABLE mkWrappedStakeValidator' #-}
mkWrappedStakeValidator' :: PubKeyHash -> Address -> BuiltinData -> BuiltinData -> ()
mkWrappedStakeValidator' pkh = wrapStakeValidator . mkStakeValidator' pkh

stakeValidator' :: PubKeyHash -> Address -> StakeValidator
stakeValidator' pkh addr = mkStakeValidatorScript $
    $$(PlutusTx.compile [|| mkWrappedStakeValidator' ||])
        `PlutusTx.applyCode` PlutusTx.liftCode pkh
        `PlutusTx.applyCode` PlutusTx.liftCode addr

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveStakeValidator' :: String -> String -> IO ()
saveStakeValidator' _pkh _bech32 = undefined
