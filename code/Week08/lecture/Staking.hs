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

module Staking
    ( stakeValidator
    , saveStakeValidator
    ) where

import           Plutus.V1.Ledger.Value (valueOf)
import           Plutus.V2.Ledger.Api   (Address, BuiltinData,
                                         ScriptContext (scriptContextPurpose, scriptContextTxInfo),
                                         ScriptPurpose (Certifying, Rewarding),
                                         StakeValidator, StakingCredential,
                                         TxInfo (txInfoOutputs, txInfoWdrl),
                                         TxOut (txOutAddress, txOutValue),
                                         adaSymbol, adaToken,
                                         mkStakeValidatorScript)
import qualified PlutusTx
import qualified PlutusTx.AssocMap      as PlutusTx
import           PlutusTx.Prelude       (AdditiveSemigroup ((+)), Bool (..),
                                         Eq ((==)), Integer,
                                         Maybe (Just, Nothing),
                                         MultiplicativeSemigroup ((*)),
                                         Ord ((>=)), Semigroup ((<>)), foldl,
                                         otherwise, traceError, traceIfFalse,
                                         ($), (.))
import           Prelude                (IO, String, ioError)
import           System.IO.Error        (userError)
import           Utilities              (tryReadAddress, wrapStakeValidator,
                                         writeStakeValidatorToFile)

{-# INLINABLE mkStakeValidator #-}
mkStakeValidator :: Address -> () -> ScriptContext -> Bool
mkStakeValidator addr () ctx = case scriptContextPurpose ctx of
    Certifying _   -> True
    Rewarding cred -> traceIfFalse "insufficient reward sharing" $ 2 * paidToAddress >= amount cred
    _              -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    amount :: StakingCredential -> Integer
    amount cred = case PlutusTx.lookup cred $ txInfoWdrl info of
        Just amt -> amt
        Nothing  -> traceError "withdrawal not found"

    paidToAddress :: Integer
    paidToAddress = foldl f 0 $ txInfoOutputs info
      where
        f :: Integer -> TxOut -> Integer
        f n o
            | txOutAddress o == addr = n + valueOf (txOutValue o) adaSymbol adaToken
            | otherwise              = n

{-# INLINABLE mkWrappedStakeValidator #-}
mkWrappedStakeValidator :: Address -> BuiltinData -> BuiltinData -> ()
mkWrappedStakeValidator = wrapStakeValidator . mkStakeValidator

stakeValidator :: Address -> StakeValidator
stakeValidator addr = mkStakeValidatorScript $
    $$(PlutusTx.compile [|| mkWrappedStakeValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode addr

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveStakeValidator :: String -> IO ()
saveStakeValidator bech32 = do
    case tryReadAddress bech32 of
        Nothing   -> ioError $ userError $ "Invalid address: " <> bech32
        Just addr -> writeStakeValidatorToFile "./assets/staking.plutus" $ stakeValidator addr
