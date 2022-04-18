{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.Homework2 where

import           Control.Monad          hiding (fmap)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (IO, Semigroup (..), Show (..), String, undefined)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

tName :: BuiltinByteString   
tName = "" -- use emptyByteString here then use the TokenName data constructor to create a TokenName
              -- Then you do not have to wrap and unwrap it when using it.

-- | Given Tx output reference, determin if minting transaction consumes the given UTxO as input
-- and the TokenName will is the empty ByteString.
{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkPolicy oref () ctx = traceIfFalse "utxo no found" hasTheTxOutRefAsInput &&
                       traceIfFalse "Wrong token name or amount" checkMintedToken
    where 
      info :: TxInfo 
      info =scriptContextTxInfo ctx 

      hasTheTxOutRefAsInput :: Bool 
      hasTheTxOutRefAsInput = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

      checkMintedToken :: Bool
      checkMintedToken = case flattenValue (txInfoMint info) of 
                          [(_, tn, amt)] -> unTokenName tn == tName && amt == 1 -- no need to unwrap it here
                          _              -> False

policy :: TxOutRef -> Scripts.MintingPolicy
policy oref = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode oref

curSymbol :: TxOutRef -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

type NFTSchema = Endpoint "mint" Address

mint :: Address -> Contract w NFTSchema Text ()
mint address = do 
  utxos <- utxosAt address 
  case Map.keys utxos of 
    []       -> Contract.logInfo @String "no utxo found"
    oref : _ -> do 
      let val     = Value.singleton (curSymbol oref) (TokenName tName) 1 -- also here wrap it.
          lookups = Constraints.mintingPolicy (policy oref) <> Constraints.unspentOutputs utxos
          tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
      ledgerTx <- submitTxConstraintsWith @Void lookups tx 
      void $ awaitTxConfirmed $ getCardanoTxId ledgerTx  
      Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

test :: IO ()
test = runEmulatorTraceIO $ do
    let w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"mint" h1 $ mockWalletAddress w1
    callEndpoint @"mint" h2 $ mockWalletAddress w2
    void $ Emulator.waitNSlots 1
