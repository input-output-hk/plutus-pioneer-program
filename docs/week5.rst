Week 05
=======

.. note::
      These is a written version of `Lecture
      #5 <https://youtu.be/6VbhY162GQA>`__.

      In this lecture we learn about native tokens, minting policies and NFTs.

      These notes use Plutus commit 0c3c310cab61dbff8cbc1998a3678b367be6815a


Overview
--------

We are going to talk about how Plutus supports native tokens and how to define under which conditions native tokens can be minted and burned. But before we get to that,
let's explore what *value* means in Cardano.

When we talked about the (E)UTxO model, we learned that each UTxO (unspent transaction) has an address and a value. And, we saw that, as a result of being extended to the (E)UTxO model, each
UTxO also has a *Datum*. We have seen examples of such UTxOs in previous lectures.

In almost all the examples we have seen so far, the value was simply an Ada value, denominated in lovelace. The exception was the first example, from lecture 1, namely the *English Auction*
example. In that example we auctioned away an NFT. However, the NFT was just created out of thin air in the playground.

In the real Cardano blockchain, however, in the beginning there are only Ada, there are no other native tokens. So, you have to do something to create new native tokens, or to burn existing ones.
In this lecture we will see how to do that.

But let's first talk above values.

Value
-----

The relevant types are defined in package *plutus-ledger-api*. The modules of interest are

.. code:: haskell

    module Plutus.V1.Ledger.Value
    module Plutus.V1.Ledger.Ada

The Value Type
~~~~~~~~~~~~~~

*Value* is defined as a map from *CurrencySymbol*s to maps from *TokenName*s to *Integers*, which sounds a bit weird and complicated.

.. code:: haskell

    newtype Value = Value { getValue :: Map.Map CurrencySymbol (Map.Map TokenName Integer) }
        deriving stock (Generic)
        deriving anyclass (ToJSON, FromJSON, Hashable, NFData)
        deriving newtype (Serialise, PlutusTx.IsData)
        deriving Pretty via (PrettyShow Value)

The first thing to note is that each native token, including Ada, is identified by two pieces of data - the *CurrencySymbol* and the *TokenName*.

A *CurrencySymbol* is a *newtype* wrapper around a *ByteString*.

.. code:: haskell

    newtype CurrencySymbol = CurrencySymbol { unCurrencySymbol :: Builtins.ByteString }
        deriving (IsString, Show, Serialise, Pretty) via LedgerBytes
        deriving stock (Generic)
        deriving newtype (Haskell.Eq, Haskell.Ord, Eq, Ord, PlutusTx.IsData)
        deriving anyclass (Hashable, ToJSONKey, FromJSONKey,  NFData)

And the same is true for *TokenName*.

.. code:: haskell

    newtype TokenName = TokenName { unTokenName :: Builtins.ByteString }
        deriving (Serialise) via LedgerBytes
        deriving stock (Generic)
        deriving newtype (Haskell.Eq, Haskell.Ord, Eq, Ord, PlutusTx.IsData)
        deriving anyclass (Hashable, NFData)
        deriving Pretty via (PrettyShow TokenName)    

So we have these two *ByteStrings* that define a coin, or, as it is also called, an *asset class*.

.. code:: haskell

    assetClass :: CurrencySymbol -> TokenName -> AssetClass
    assetClass s t = AssetClass (s, t)

Ada is one asset class, and custom native tokens will be other asset classes.

A *Value* simply shows how many units exist for a given asset class.

Let's start the REPL and import the two relevant modules.

.. code:: haskell
        
    cabal repl
    Prelude Week05.Free> import Plutus.V1.Ledger.Ada
    Prelude Plutus.V1.Ledger.Ada Week05.Free> import Plutus.V1.Ledger.Value 
    Prelude Plutus.V1.Ledger.Ada Plutus.V1.Ledger.Value Week05.Free> 
    Prelude Plutus.V1.Ledger.Ada Plutus.V1.Ledger.Value Week05.Free> :set -XOverloadedStrings

.. note::
    
    We have also activated the *OverloadedStrings* extension so that we can enter *ByteString*s as literal strings.

Now let's look at some values. Let's start with lovelace values. In the *Ledger.Ada* module there is a function called *adaSymbol*.

.. code:: haskell

    Prelude Plutus.V1.Ledger.Ada Plutus.V1.Ledger.Value Week05.Free> :t adaSymbol
    adaSymbol :: CurrencySymbol
    
This gives us the currency symbol of the Ada asset class, which is just the empty *ByteString*. Similarly, there is a function *adaToken*, which will give us the token name.

.. code:: haskell

    Prelude Plutus.V1.Ledger.Ada Plutus.V1.Ledger.Value Week05.Free> :t adaToken
    adaToken :: TokenName

Again, this is also the empty *ByteString*.

We have seen before in the examples how to construct a *Value* containing just lovelace. There is a function *lovelaceValueOf* that, given an *Integer*, gives us a *Value*.

.. code:: haskell

    Prelude Plutus.V1.Ledger.Ada Plutus.V1.Ledger.Value Week05.Free> :t lovelaceValueOf
    lovelaceValueOf :: Integer -> Value
    
So, for example to have 123 lovelace, we can do:

.. code:: haskell

    Prelude Plutus.V1.Ledger.Ada Plutus.V1.Ledger.Value Week05.Free> lovelaceValueOf 123
    Value (Map [(,Map [("",123)])])

You will always use a helper function such as *lovelaceValueOf* to construct the value maps - you would never need to construct one directly.

Here we see the map. The out map of currency symbols has one key, which is the empty symbol for Ada, and the inner map of token names has one key, the empty string for Ada,
and a value of 123.

One thing we can do with values is combine them. The *Value* class is an instance of *Monoid*, so we can use *mappend*, which we can write as *<>*, which comes from a super class of
*Monoid* called *Semigroup*.

.. code:: haskell

    Prelude Plutus.V1.Ledger.Ada Plutus.V1.Ledger.Value Week05.Free> lovelaceValueOf 123 <> lovelaceValueOf 10
    Value (Map [(,Map [("",133)])])
    
So, how do we create *Value*s containing native tokens?

There is a very useful function called *singleton*.

.. code:: haskell

    Prelude Plutus.V1.Ledger.Ada Plutus.V1.Ledger.Value Week05.Free> :t singleton
    singleton :: CurrencySymbol -> TokenName -> Integer -> Value

This will create a *Value* for a token specified by the *CurrencySymbol* and the *TokenName*, and for a given *Integer* amount.

.. code:: haskell

    Week05.Free> singleton "a8ff" "ABC" 7
    Value (Map [(a8ff,Map [("ABC",7)])])

The first argument, "a8ff" for *CurrencySymbol" has to be a string representing a hexadecimal value, for reasons that will soon become clear. The second argument, "ABC"
for *TokenName* can be an arbitrary string.

And, we can combine, as before, with the *mappend* operator. We can now create a somewhat more interesting map.

.. code:: haskell

    Week05.Free> singleton "a8ff" "ABC" 7 <> lovelaceValueOf 42 <> singleton "a8ff" "XYZ" 100
    Value (Map [(,Map [("",42)]),(a8ff,Map [("ABC",7),("XYZ",100)])])
    
Now, we see a map representing 42 lovelace as well as two tokens *ABC* and *XYZ* both belonging to the *CurrencySymbol* "af88", and each with their respective integer amounts.

Let's give this value a name:

.. code:: haskell

    Week05.Free> let v = singleton "a8ff" "ABC" 7 <> lovelaceValueOf 42 <> singleton "a8ff" "XYZ" 100
    Week05.Free> v
    Value (Map [(,Map [("",42)]),(a8ff,Map [("ABC",7),("XYZ",100)])])
    
Another useful function is *valueOf* which allows us to get the value of a given currency symbol and token name.

.. code:: haskell

    Week05.Free> :t valueOf
    valueOf :: Value -> CurrencySymbol -> TokenName -> Integer

    Week05.Free> valueOf v "a8ff" "XYZ"
    100    
    
    Week05.Free> valueOf v "a8ff" "ABC"
    7

    Week05.Free> valueOf v "a8ff" "abc"
    0

Another useful function is *flattenValue*. As the name suggests, it flattens the map of maps into a flat list of triples.

 .. code:: haskell

    Week05.Free> :t flattenValue
    flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)]

    Week05.Free> flattenValue v
    [(a8ff,"ABC",7),(a8ff,"XYZ",100),(,"",42)]

Minting Policies
----------------

Now the question is why? Why do we need both a currency symbol and a token name? Why don't we just use one identifier for an asset class? And why does the currency
symbol have to be in hexadecimal digits?

This is where so-called minting policies come in.

The rule is that, in general, a transaction can't create or delete tokens. Everything that goes in also comes out, with the exception of the fees. There is always a lovelace feel
that has to be paid with each transaction. The fee depends on the size of the transaction and the number of steps that the validation script takes to execute, and the memory
consumption of the script.

But, if that was the whole story then we could never create native tokens. And this is where minting policies come in, and the relevance of the currency symbol comes in.

The reason that the currency symbol has to consist of hexadecimal digits is that it is actually the hash of a script. And this script is called the minting policy, and if
we have a transaction where we ant to create native or burn native tokens then, for each native token that we try to create or burn, the currency symbol is looked up. So, the
corresponding script must also be contained in the transaction. And that script is executed along with the other validation scripts.

And, similar to the validation scripts that we have seen so that validate input, the purpose of these minting scripts is to decide whether this transaction has the right to
mint or burn tokens. Ada also fits into this scheme. Remember the the currency symbol of Ada is just an empty string, which is not the hash of any scripts. So there is no
script that hashes to the empty string, so there is no script that would allow the minting or burning of Ada, which means that Ada can never be minted or burned.

All the Ada that exists comes from the Genesis transaction and the total amount of Ada in the system is fixed and can never change. Only custom native tokens can have custom minting policies.

So we'll look at an example of a minting policy next and will see that it is very similar to a validation script, but not identical.

Before we write out first minting policy, let's briefly recall how validation works.

When we don't have a public key address, but a script address, and a UTxO that sits at that address, then for any transaction that tries to consume that UTxO, a validation script is run.

That validation script gets, as input, the datum, which comes from the UTxO, the redeemer, which comes from the input, and the context.

Recall that the *ScriptContext* has two fields.

.. code:: haskell

    data ScriptContext = ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }

One of those fields is *ScriptPurpose*, and, for this field, everything we have seen until now has been of type *Spending*.

.. code:: haskell

    data ScriptPurpose
        = Minting CurrencySymbol
        | Spending TxOutRef
        | Rewarding StakingCredential
        | Certifying DCert
    
The other field is of type *TxInfo* which contains all the context information about the transaction.

.. code:: haskell

    -- | A pending transaction. This is the view as seen by validator scripts, so some details are stripped out.
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
        
For minting policies, this is triggered if the *txInfoForge* field of the transaction contains a non-zero value. In all of the transactions we have seen so far, this field value
has been zero - we have never created or destroyed any tokens.

If it *is* non-zero, then for each currency symbol contained in the *Value*, the corresponding minting policy script is run. 

Whereas the validation scripts had three inputs - the datum, the redeemer and the context, these minting policy scripts only have one input - the context. 
And it is the same context as we had before - the *ScriptContext*. It would make no sense to have the datum, as it belongs to the UTxO, and it would make no sense to have
the redeemer as it belongs to the validation script. The minting policy belongs to the transaction itself, not to a specific input or output.

As for the *ScriptPurpose*, this will not be *Spending* as it has been until now, but will be *Minting*.

Example 1 - Free
----------------

Let's write a simple minting policy.

On chain
~~~~~~~~

When we wrote a validator we had a function such as the following:

.. code:: haskell

    mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool

We also saw the low-level version where we had three *Data* arguments and returned *Unit*. And we saw that there can be additional arguments before the datum, if we
write a parameterized script.

We can also have parameterized minting policy scripts and we will see that in a later example. But first we will look at one that is not parameterized.

First, let's rename the function to *mkPolicy*, remove the datum and redeemer, and write the simplest minting policy that we can.

.. code:: haskell

    mkPolicy :: ScriptContext -> Bool
    mkPolicy _ = True

This policy ignores the context and always returns *True*. This will allow arbitrary minting and burning of tokens for and token name that belongs to the currency symbol
associated with this policy.

Remember that, when we were writing a validator, we needed to use Template Haskell to compile this function to Plutus code. We need to do something similar for our minting policy.

.. code:: haskell

    policy :: Scripts.MonetaryPolicy
    policy = mkMonetaryPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy mkPolicy ||])
    
And, as before, we need to make the *mkPolicy* function *INLINABLE*, as everything within the Oxford brackets needs to be available at compile time.


.. code:: haskell

    {-# INLINABLE mkPolicy #-}
    mkPolicy :: ScriptContext -> Bool
    mkPolicy _ = True

Now that we have a policy, we can get a currency symbol from the policy.

.. code:: haskell

    curSymbol :: CurrencySymbol
    curSymbol = scriptCurrencySymbol policy
    
And, we can look at this in the REPL:

.. code:: haskell

    Prelude Week05.Free> curSymbol
    e01824b4319351c40b5ec727fff328a82076b1474a6bad6c8e8a2cd835cc6aaf

And this completes the on-chain part, for this simple minting policy. But in order to try it out and interact with it, we need an off-chain part.

Off chain
~~~~~~~~~

What should the off-chain part do? Well, it should allow arbitrary wallets to mint and burn tokens of this currency symbol.

We have the currency symbol, so what is missing is the token name and the amount we want to mint or burn. And for this, we will define a data type *MintParams*.

.. code:: haskell

    data MintParams = MintParams
        { mpTokenName :: !TokenName
        , mpAmount    :: !Integer
        } deriving (Generic, ToJSON, FromJSON, ToSchema)    

We see two fields - *mpTokenName* and *mpAmount*. The idea is that if the *mpAmount* is positive, we should create tokens, and if it is negative, we should burn tokens.

The next step is to define the schema. Recall that one of the parameters of the *Contact* monad was the schema that defined the available actions that we can take.

.. code:: haskell

    type FreeSchema =
        BlockchainActions
            .\/ Endpoint "mint" MintParams
        
As always, we have *BlockchainActions* that give us access generic things like getting your own public key. And here, we have added an endpoint *mint* using the type-level operator
we have seen previously.

So, now we can look at the contract itself.

.. code:: haskell

    mint :: MintParams -> Contract w FreeSchema Text ()
        
In the past, we have not gone into detail with the off-chain part of the contract. But, as we now know about the *Contract* monad from the last lecture, we are ready to go into it
in much more detail.

Recall that the *Contract* monad takes four type parameters. 

The first is the writer monad which allows us to use a *tell* function. By leaving this parametric with a small
*w*, we indicate that we will not be making use of this parameter - we won't *tell* any state.

The next parameter is the schema that we just discussed. As noted above, by using *FreeSchema* we have access to the regular block chain actions, as well as the *mint* endpoint.

The third parameter is the type of error message, and as we have seen, *Text* is usually a good choice.

Finally the last parameter is the return type, and our contract will just have the Unit return type.

Now the function body. As *Contact* is a monad, we can use *do* notation.

.. code:: haskell

    mint mp = do
        let val     = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp)
            lookups = Constraints.monetaryPolicy policy
            tx      = Constraints.mustForgeValue val
        ledgerTx <- submitTxConstraintsWith @Void lookups tx
        void $ awaitTxConfirmed $ txId ledgerTx
        Contract.logInfo @String $ printf "forged %s" (show val)
        
The first thing that we define is the value that we want to forge. For this we are using the *singleton* function that we tried out in the REPL earlier.

The arguments to the *singleton* function are the currency symbol that represents the hash of the minting policy, plus the token name and amount extracted from the *MintParams*.

We'll skip the *lookups* assignment for the moment, and move onto the *tx* assignment.

One of the main purposes of the *Contract* monad is to construct and submit transactions. The path that the Plutus team has taken to do that is provide a way to specify
the constraints of the transaction you are defining. The Plutus libraries then take care of constructing the correct transaction (if possible). This is as opposed to being require to specify
all the inputs and outputs manually, which would be tedious as many requirements, such as sending change back to the sending wallet, are often the same.

These conditions all have names that start with *must*. There are things like *mustSpendScriptOutput*, *mustPayToPublicKey* and all sorts of conditions that can be put 
on a condition.

In our example, we are using *mustForgeValue* and we pass it the previously-defined *val*. The result of forging the tokens specified by *val* is that they will end up 
in our own wallet.

Once the conditions are defined, you then need to call a function to submit the transaction. There are a variety of such functions, but in this case, the appropriate one
is *submitTxConstraintsWith*. 

These *submitTx* functions all take these declarative conditions that the transaction must satisfy, and then they try to construct a transaction that 
fulfils those conditions. In our case, the only condition is that we want to forge the value.

So what must the *submitTxConstraintsWith* do in order to create a valid transaction? It must, for example balance the inputs and outputs. In this case, because we always 
have transaction fees, we need an input that covers the transactions fees. So, to create the transaction, the function will look at our own UTxOs and find one, or more, that can
cover the transaction fees, and use them as an input to the transaction.

Furthermore, if we are forging value (it the *mpAmount* is positive), that must go somewhere.




    










 
 



    




