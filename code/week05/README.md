# Lecture 5 notes
Lecture notes based on the first ever official [Plutus-Pioneer program](https://github.com/input-output-hk/plutus-pioneer-program). This notes follow the [YouTube lecture 4](https://www.youtube.com/watch?v=6VbhY162GQA).

## 1. Native Token Values

To mint or burn Native tokens we first need to discuss about values. The relevant value-types are define in [`Value.hs`](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Value.hs) & [`Ada.hs`](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Ada.hs). Inside `Value.hs` we can find 

it is important to mention that any Native Token, including ADA is identified two things: i) `CurrencySymbol` & ii) `TokenName` as seen below

    -- See note [Currencies] for more details.                                              -- line 209
    newtype Value = Value { getValue :: Map.Map CurrencySymbol (Map.Map TokenName Integer) }
        deriving stock (Generic)
        deriving anyclass (ToJSON, FromJSON, Hashable, NFData)
        deriving newtype (Serialise, PlutusTx.IsData)
        deriving Pretty via (PrettyShow Value)

both are **new-type `AssetClass`** which is a **wrapper** around bytestrings. 


Lets play a little bit with this in the cabal repl

    repl$ import Plutus.V1.Ledger.Value
    repl$ import Plutus.V1.Ledger.Ada
    repl$ ada
    repl$ adaToken
    
as we can see `adaSymbol` gives the emptysymbol for ADA & `adaToken` gives us an empty string. So ADA is created with those two pieces. However, to create cyrrency value we need special functions. One of them is `lovelaceValueOf` which can map an integer and map it into a value.

It is possble to combine values (e.g. `x` & `y`) using 

    lovelaceValueOf x <> lovelaceValueOf y
    
this can be cpmbined because **value-class** is an **instance of Monoid** and we have seen in the previous class that Monoids are things that we can combine because they contain a mutual element called **`mempty`** and the combination occurs thanks to **`mappend`** and the operator for this is the `<>`

## 1.1 First Native Token

Now that we know what we need to define a Native Token and also how to map an integer to a proper token value, we can try in the repl, e.g. token *Uno* and value 1 (we aslo will need to specify Hexadecimal byte indetifier, here we put arbitrarily: "a8ff")

    repl$ :t singleton
    repl$ singleton "a8ff" "Uno" 1
    repl$ let vv = singleton "a8ff" "Uno" 1 <> lovelaceValueOf 42 <> singleton "a8ff" "OMEGA" 77
    
In the las line we have defined a combination of different Native Tokens: Ada, Uno & OMEGA into the variable `vv`. To filter the value of the corresponding token we can ask using `valueOf`

    repl$ valueOf vv "" ""           -- gets Ada value
    repl$ valueOf vv "a8ff" "Uno"    -- gets Uno value
    repl$ valueOf vv "a8ff" "OMEGA"  -- gets OMEGA value
    
Now the question arises: *Why we need two identifiers? (`CurrencySymbol` & `TokenName`)* and *Why hexadecimal?* this has to do with **Minting policies** 

1) A transaction can't create or delete tokens (convervation of UTXOs - fees)
2) The reason of why the currency symbol-bytestring is in hexadecimal numbers is because it is the hash of a Script and this is called the *Minting Policie*

This means that whenever a token is to be minted or burned the Script can be reached through its hexadecimal hash. So in the case of Ada, since its identifiers are both empty strings `""`, there is no hash to be reached, thus it can not be minted or burned. All the Ada that will ever exist comes from the genesis block. Only custom Native Tokens can be created/minted or burned under certain personalized conditions.

## 2. Example of Minting policy

#### 2.1 Refresher on Validation
Before diving into the example lets refresh our minds on how a validation works.

When instead of having a Public-Key-address we have a Script-Address and a UTXO that sits at the Script-Address and we also have a transaction that attempts to consume the UTXO as an input then for each Script input the validation Script runs

This validation Script gets i) Datum ii) Redeemer & iii) Context. The latter can be found in [`Context.hs`](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Contexts.hs), defined in line 116 which needs the data-types defined in lines 93 & 100 as seen below

    -- | Purpose of the script that is currently running -- line 93
    data ScriptPurpose
        = Minting CurrencySymbol
        | Spending TxOutRef
        | Rewarding StakingCredential
        | Certifying DCert

    -- | A pending transaction. This is the view as seen by validator scripts, so some details are stripped out. -- line 100
    data TxInfo = TxInfo
        { txInfoInputs      :: [TxInInfo] -- ^ Transaction inputs
        , txInfoInputsFees  :: [TxInInfo]     -- ^ Transaction inputs designated to pay fees
        , txInfoOutputs     :: [TxOut] -- ^ Transaction outputs
        , txInfoFee         :: Value -- ^ The fee paid by this transaction.
        , txInfoForge       :: Value -- ^ The 'Value' forged by this transaction.
        , txInfoDCert       :: [DCert] -- ^ Digests of certificates included in this transaction
        , txInfoWdrl        :: [(StakingCredential, Integer)] -- ^ Withdrawals
        , txInfoValidRange  :: SlotRange -- ^ The valid range for the transaction.
        , txInfoSignatories :: [PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
        , txInfoData        :: [(DatumHash, Datum)]
        , txInfoId          :: TxId
        -- ^ Hash of the pending transaction (excluding witnesses)
        } deriving (Generic)

    data ScriptContext = ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose } -- line 116

The `ScriptPurpose`as of this lecture always had the purpose of `Spending TxOutRef` with a reference of the UTXO we are trying to consume. Whereas the `TxInfo` has all the information of the context that is being validated`. 

In minting policies the `txInfoForge` is triggered and will have a non-zero value (for different asset classes) in contrast to regular Ada trasactions. 

Each currency symbol is the hash of the Script














































