Week 05 - Native Tokens
=======================

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

Furthermore, if we are forging value (if *mpAmount* is positive), that must go somewhere. In this case, *submitTxConstraintsWith*, will create an output that sends the 
newly-minted value to our own wallet.
 
If, on the other hand, we were burning tokens (if *mpAmount* is negative), then those tokens must come from somewhere. In that case, the *submitTxConstraintsWith* function
would find an input in our own wallet from which to take the tokens.

The submit function can also fail. For example, if we want to pay someone, but we do not have enough funds in our wallet, it would fail. Or, if we are asking to burn tokens 
that we don't have, it will also fail. On failure, an exception would be thrown, with an error message of type *Text*.

Now, back to the *lookups*. In order to fulfil the conditions in the *mustForgeValue* function, and to construct the transaction, sometimes the library needs additional information.
In this case, in order to validate a transaction that forges value, the nodes that validate the transaction have to run the policy script.

But, the currency symbol is only the hash of the policy script. In order to run the script itself, it must be included in the transaction. Which means that, in the 
construction step of the transaction, when the algorithm see the *mustForgeValue* constraint, it knows it has to attach the corresponding policy script to the transaction.

In order to tell the algorithm where the policy script is, we can give it hints, and these are the lookups. The are a variety of lookups that can be used - you can give 
UTxOs, validator scripts, and, as we do here, you can give monetary policy scripts.

In our case, the only thing we need to supply as a lookup is the policy that we defined earlier in the script.

There are variants of *submitTxConstraintsWith* without the *with* that do not take lookups, as we have seen in previous lectures.

Finally, the *@Void* on the line:

.. code:: haskell

    ledgerTx <- submitTxConstraintsWith @Void lookups tx

Most of the constraint functions are geared towards using a specific validator script. Normally you have the situation that you are working on one specific smart contract.
And that specific smart contract has a datum and a redeemer type, and most of the constraints functions are parametric in the datum and redeemer type. In that case you
can directly use the datum type without first having to convert it to the Plutus *Datum* type.

But in this case, we are not making use of that. We don't have any validator script. Which means that *submitTxConstraintsWith* wouldn't know which type to use for datum and
redeemer because we don't have them in this example. So, in that case we must tell the compiler which type to use. We don't care, as there is no datum and redeemer, so we
use the *Void* type.

Also, in the same line, we see a monadic bind, so we know that this is a monadic action happening within the *Contract* monad. The reason for this is that, in order to lookup,
for example, our UTxOs, the *submitTxConstraintsWith* function must make use of the super power of the *Contract* monad, which is to access the *BlockchainActions*.

Now, *ledgerTx* is basically a handle to the transaction to we just submitted.

Then we wait for the transaction to be confirmed.

.. code:: haskell

    void $ awaitTxConfirmed $ txId ledgerTx

Currently, if the transaction validation fails, the await for confirmation line will block forever. However, this will soon change in an upcoming Plutus release to allow
us to listen for status changes, so you could detect if validation failed.

Once confirmed, we simply write a log message.

Finally, we need some more boilerplate to define our endpoint, to be able to actually execute the *mint* function, for example, in the playground.

.. code:: haskell

    endpoints :: Contract () FreeSchema Text ()
    endpoints = mint' >> endpoints
      where
        mint' = endpoint @"mint" >>= mint    

We define another contract, *endpoints*, and that is always the name of the contract that the playground will run. So, if you want to test something in the playground, you
always need something called *endpoints*.

Here we just define a function called *mint'* and then recursively call *endpoints*, so once it has executed, it will be available to be executed again.

For *mint'* we must somehow get the *MintParams* and for that we use the *endpoint* function. The *endpoint* function blocks until someone provides a parameter. Once the
parameter of *MintParams* is provided, we use the monadic bind to call the *mint* function with those arguments.

The final two lines, as we have seen before, are just needed for the playground UI.

.. code:: haskell

    mkSchemaDefinitions ''FreeSchema
    mkKnownCurrencies []    

In The Playground  
~~~~~~~~~~~~~~~~~

We have set up a scenario where Wallet 1 mints 555 ABC tokens, and Wallet 2 mints 444 ABC tokens. Then, after waiting for 1 slot, Wallet 1 burns 222 ABC tokens. Finally, we
wait for 1 slot at the end.

.. figure:: img/week05__00007.png

Now, if we evaluate this, first we see the genesis transaction where the wallets are given 1000 lovelace each.

.. figure:: img/week05__00008.png

Next, we see two transactions at Slot 1. The first is the transaction from Wallet 2, where 444 ABC tokens are minted, and a 10 lovelace fee is paid. The UTxO to pay the fees 
was automatically found by the function that created the transaction *submitTxConstraintsWith*, as discussed previously.

We see something here that we have not seen before - the *Forge* part of a transaction, where the native tokens are actually created. The box contains the currency symbol (the 
policy hash) and the token name.

We also see the two outputs - once with the 990 lovelace change, and another with the newly-minted tokens. These outputs could, in fact, be combined, but here they are shown 
as two separate UTxOs.

.. figure:: img/week05__00009.png

Then, we see the transaction from Wallet 1, where 555 ABC tokens are minted, and a 10 lovelace fee is paid.

.. figure:: img/week05__00010.png

Finally, we see the burning of 222 tokens by Wallet 1. Here we see that the algorithm did something slightly different. When it notices that a burn is taking place, it has
found the ABC tokens UTxO in Wallet 1 and used them as an input. We also note here that the output UTxO is combined, which, as we mentioned above, can be done instead of
using two output UTxOs.

.. figure:: img/week05__00011.png

And we can also view the final balances to double check that all went according to plan.

.. figure:: img/week05__00012.png

With our monetary policy, we can create arbitrary forging and burning transactions by any wallet. So, this is probably not a very good monetary policy. The purpose of a
token is to represent value, but if anybody at any time can mint new tokens, this token will not make much sense. There might be some exotic use case for it, but realistically
this policy is rather useless.

Testing with EmulatorTrace
~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's also test this from the command line, rather than in the playground.

.. code:: haskell

    test :: IO ()
    test = runEmulatorTraceIO $ do
        let tn = "ABC"
        h1 <- activateContractWallet (Wallet 1) endpoints
        h2 <- activateContractWallet (Wallet 2) endpoints
        callEndpoint @"mint" h1 $ MintParams
            { mpTokenName = tn
            , mpAmount    = 555
            }
        callEndpoint @"mint" h2 $ MintParams
            { mpTokenName = tn
            , mpAmount    = 444
            }
        void $ Emulator.waitNSlots 1
        callEndpoint @"mint" h1 $ MintParams
            { mpTokenName = tn
            , mpAmount    = -222
            }
        void $ Emulator.waitNSlots 1

If we run this in the REPL, we see what we saw in the playground, but instead on the console. It's not as pretty, but it is quicker.

.. code::

    Prelude Week05.Free> test
    Slot 00000: TxnValidate af5e6d25b5ecb26185289a03d50786b7ac4425b21849143ed7e18bcd70dc4db8
    Slot 00000: SlotAdd Slot 1
    Slot 00001: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
    Contract instance started
    Slot 00001: 00000000-0000-4000-8000-000000000001 {Contract instance for wallet 2}:
    Contract instance started
    Slot 00001: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
    Receive endpoint call: Object (fromList [("tag",String "mint"),("value",Object (fromList [("unEndpointValue",Object (fromList [("mpAmount",Number 555.0),("mpTokenName",Object (fromList [("unTokenName",String "ABC")]))]))]))])
    Slot 00001: W1: TxSubmit: 7c01d39fc031815eaf05d97709e4973a24dfa38e9dd68a4fd1ec92bb80cf76e4
    Slot 00001: 00000000-0000-4000-8000-000000000001 {Contract instance for wallet 2}:
    Receive endpoint call: Object (fromList [("tag",String "mint"),("value",Object (fromList [("unEndpointValue",Object (fromList [("mpAmount",Number 444.0),("mpTokenName",Object (fromList [("unTokenName",String "ABC")]))]))]))])
    Slot 00001: W2: TxSubmit: 6ba7eb4441992284e687d184080d4a8693e7b188fc45150d6e7ccd1243968f53
    Slot 00001: TxnValidate 6ba7eb4441992284e687d184080d4a8693e7b188fc45150d6e7ccd1243968f53
    Slot 00001: TxnValidate 7c01d39fc031815eaf05d97709e4973a24dfa38e9dd68a4fd1ec92bb80cf76e4
    Slot 00001: SlotAdd Slot 2
    Slot 00002: *** CONTRACT LOG: "forged Value (Map [(e01824b4319351c40b5ec727fff328a82076b1474a6bad6c8e8a2cd835cc6aaf,Map [(\"ABC\",555)])])"
    Slot 00002: *** CONTRACT LOG: "forged Value (Map [(e01824b4319351c40b5ec727fff328a82076b1474a6bad6c8e8a2cd835cc6aaf,Map [(\"ABC\",444)])])"
    Slot 00002: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
    Receive endpoint call: Object (fromList [("tag",String "mint"),("value",Object (fromList [("unEndpointValue",Object (fromList [("mpAmount",Number -222.0),("mpTokenName",Object (fromList [("unTokenName",String "ABC")]))]))]))])
    Slot 00002: W1: TxSubmit: 95d42e93ee41ab5bed7857b176be5a4e16602323eaacaa90f3bb807a9fd235c0
    Slot 00002: TxnValidate 95d42e93ee41ab5bed7857b176be5a4e16602323eaacaa90f3bb807a9fd235c0
    Slot 00002: SlotAdd Slot 3
    Slot 00003: *** CONTRACT LOG: "forged Value (Map [(e01824b4319351c40b5ec727fff328a82076b1474a6bad6c8e8a2cd835cc6aaf,Map [(\"ABC\",-222)])])"
    Slot 00003: SlotAdd Slot 4
    Final balances
    Wallet 1: 
        {, ""}: 99999980
        {e01824b4319351c40b5ec727fff328a82076b1474a6bad6c8e8a2cd835cc6aaf, "ABC"}: 333
    Wallet 2: 
        {e01824b4319351c40b5ec727fff328a82076b1474a6bad6c8e8a2cd835cc6aaf, "ABC"}: 444
        {, ""}: 99999990
    ...    
    Wallet 10: 
        {, ""}: 100000000

Example 2 - Signed
------------------

On-chain
~~~~~~~~

Let's look at a more realistic example.

We'll take a copy of the Free module, and call it Signed.

Probably the easiest example of a realistic minting policy is one where the minting and burning of tokens is restricted to transactions that are signed by a specific
public key hash. That is similar to a central bank, in fiat currencies.

This means that our policies is no longer without parameters. We need the public key hash.  In addition, we are going to need to look at the context, so we can't just ignore it like last time.

We recall that *scriptContextTxInfo* from the context contains a list of all the signatories of the transaction. So, we can use this to see if the required signatory is one
of them.

.. code:: haskell

    mkPolicy :: PubKeyHash -> ScriptContext -> Bool
    mkPolicy pkh ctx = txSignedBy (scriptContextTxInfo ctx) pkh

The *txSignedBy* function is a convenient way of checking this. In previous examples, we used the *elem* function to check that it existed in the list.

.. code:: haskell

    Prelude Week05.Free> import Ledger
    Prelude Ledger Week05.Free> :t txSignedBy
    txSignedBy :: TxInfo -> PubKeyHash -> Bool

Now, we need to update the part of the code that compiles our *mkPolicy* function into Plutus code. We will use the same techniques that we have used when writing
validator scripts. Specifically, we use the *applyCode* function to allows us to reference *pkh*, whose value is only known at runtime.

.. code:: haskell

    policy :: PubKeyHash -> Scripts.MonetaryPolicy
    policy pkh = mkMonetaryPolicyScript $
        $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy . mkPolicy ||])
        `PlutusTx.applyCode`
        PlutusTx.liftCode pkh

We also need to update the *curSymbol* function, as it now depends on the public key hash. It depends on it so that it can pass it to the *policy* function.

.. code:: haskell

    curSymbol :: PubKeyHash -> CurrencySymbol
    curSymbol = scriptCurrencySymbol . policy

Note, the second line here, the body, is a shorter way of writing:

.. code:: haskell

   curSymbol pkh = scriptCurrencySymbol $ policy pkh

This is clear, when you consider something like the following, where *timesSix* is just another way of writing the results of combining the functions *timesTwo* and *timesThree*.

.. code:: haskell

   timesSix x = timesTwo $ timesThree x 

is exactly the same as...

.. code:: haskell

   timesSix = timesTwo . timesThree

This process of simplification is called ETA reduction, so if you ever see your IDE hinting that you can ETA reduce, this is what it's talking about.

Now for the off-chain code.

Off-chain
~~~~~~~~~

We don't need to extend the *MintParams* data type for the off-chain code. A wallet that wants to mint or burn a currency can sign with its own public key hash. This is the only 
signature that a wallet can provide, and it has the ability to look it up for itself.

We will make a change to the name of the schema for clarity. We'll also, of course, update this name wherever it appears in the contract script.

.. code:: haskell

    type SignedSchema =
        BlockchainActions
            .\/ Endpoint "mint" MintParams


Now, for the *mint* function, we need to pass the public key hash to the *curSymbol* function. Getting hold of the public key is something that is provided by 
*BlockchainActions*. So, we will get this from *Contract* and apply the *pubKeyHash* function to it.

One way to do this would be

.. code:: haskell

    pk <- Contract.ownPubKey
    let pkh = pubKeyHash pk

However, as *Contract* is a monad, and therefore an instance of *Functor*, we have the *fmap* function available, which will turn a *Contract a* into a *Contract b*. In
this case we can take advantage of that by using the *pubKeyHash* function as the (a -> b) function of fmap and this will turn *Contract pubKey* into *Contract pubKeyHash*,
and then we can grab this value instead.

.. code:: haskell

    pkh <- fmap pubKeyHash Contract.ownPubKey

There is one more thing we can do to improve this. There is an operator for *fmap*.    

.. code:: haskell

    pkh <- pubKeyHash <$> Contract.ownPubKey

.. code:: haskell

Ok, now let's update the lookups line to pass in the public key hash.

.. code:: haskell

    lookups = Constraints.monetaryPolicy $ policy pkh

And now we have finished modifying the *mint* function.

.. code:: haskell

    mint :: MintParams -> Contract w SignedSchema Text ()
    mint mp = do
        pkh <- pubKeyHash <$> Contract.ownPubKey
        let val     = Value.singleton (curSymbol pkh) (mpTokenName mp) (mpAmount mp)
            lookups = Constraints.monetaryPolicy $ policy pkh
            tx      = Constraints.mustForgeValue val
        ledgerTx <- submitTxConstraintsWith @Void lookups tx
        void $ awaitTxConfirmed $ txId ledgerTx
        Contract.logInfo @String $ printf "forged %s" (show val)

So, let's try it out using the *test* function.

.. code::

    Prelude Ledger Week05.Signed> Week05.Signed.test
    Slot 00000: TxnValidate af5e6d25b5ecb26185289a03d50786b7ac4425b21849143ed7e18bcd70dc4db8
    Slot 00000: SlotAdd Slot 1
    Slot 00001: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
    Contract instance started
    Slot 00001: 00000000-0000-4000-8000-000000000001 {Contract instance for wallet 2}:
    Contract instance started
    Slot 00001: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
    Receive endpoint call: Object (fromList [("tag",String "mint"),("value",Object (fromList [("unEndpointValue",Object (fromList [("mpAmount",Number 555.0),("mpTokenName",Object (fromList [("unTokenName",String "ABC")]))]))]))])
    Slot 00001: W1: TxSubmit: 20289e7b1bb6692b35e24e0f9293327f9169d843ae0ea431186fdefae6092a44
    Slot 00001: 00000000-0000-4000-8000-000000000001 {Contract instance for wallet 2}:
    Receive endpoint call: Object (fromList [("tag",String "mint"),("value",Object (fromList [("unEndpointValue",Object (fromList [("mpAmount",Number 444.0),("mpTokenName",Object (fromList [("unTokenName",String "ABC")]))]))]))])
    Slot 00001: W2: TxSubmit: 1c367cf81dd2da478abb96235ee16facf9f7d47374c9455d5fdd516aaf04d0c2
    Slot 00001: TxnValidate 1c367cf81dd2da478abb96235ee16facf9f7d47374c9455d5fdd516aaf04d0c2
    Slot 00001: TxnValidate 20289e7b1bb6692b35e24e0f9293327f9169d843ae0ea431186fdefae6092a44
    Slot 00001: SlotAdd Slot 2
    Slot 00002: *** CONTRACT LOG: "forged Value (Map [(7183b1cf81e44b26c558ddf442c4a7161a1b504b61136a8773dc2e4960323521,Map [(\"ABC\",555)])])"
    Slot 00002: *** CONTRACT LOG: "forged Value (Map [(2a964fa6314803cf1b61165aeb1d758e355aae9480a29e282b58e76983f101ba,Map [(\"ABC\",444)])])"
    Slot 00002: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
    Receive endpoint call: Object (fromList [("tag",String "mint"),("value",Object (fromList [("unEndpointValue",Object (fromList [("mpAmount",Number -222.0),("mpTokenName",Object (fromList [("unTokenName",String "ABC")]))]))]))])
    Slot 00002: W1: TxSubmit: 6e20d243447d7f49de509ef6b52c6d947769d95a6451c9cda53e42a0ba02fa69
    Slot 00002: TxnValidate 6e20d243447d7f49de509ef6b52c6d947769d95a6451c9cda53e42a0ba02fa69
    Slot 00002: SlotAdd Slot 3
    Slot 00003: *** CONTRACT LOG: "forged Value (Map [(7183b1cf81e44b26c558ddf442c4a7161a1b504b61136a8773dc2e4960323521,Map [(\"ABC\",-222)])])"
    Slot 00003: SlotAdd Slot 4
    Final balances
    Wallet 1: 
        {, ""}: 99999980
        {7183b1cf81e44b26c558ddf442c4a7161a1b504b61136a8773dc2e4960323521, "ABC"}: 333
    Wallet 2: 
        {2a964fa6314803cf1b61165aeb1d758e355aae9480a29e282b58e76983f101ba, "ABC"}: 444
        {, ""}: 99999990
    ...
    Wallet 10: 
        {, ""}: 100000000

This looks very similar to before, but this time, notice that, while the token names are the same, the currency symbols are different for each wallet.

NFTs
----

Let's now talk about NFTs - Non-Fungible Tokens. NFTs are tokens that have a quantity of exactly 1.

The examples of native tokens that we have studied so far are definitely not NFTs because we could easily mint as many as we wanted. This is true not only in the first
example where anyone could mint tokens, but also in the second example, where, so long as you are the owner of the correct public key hash, you could mint unlimited tokens
for the associated currency symbol and token name.

In order to produce an NFT, perhaps the first naive idea would be to look at forge field in the policy and enforce a policy where the amount is one.

But that wouldn't help us. That would only mean that during one transaction you can mint only one token. But nobody could stop us from submitting as many of those transactions
as we like.

The second option is actually in use already on the Cardano blockchain. NFTs have been available since the Mary fork, which predates Plutus, and to do this, they are implemented
using deadlines.

We saw in previous examples how time can be incorporated in validation scripts, and the same can be done in policy scripts. 

The idea here is to only allow minting before a given deadline has passed. Using this method, if you want to mint an NFT, you mint one token before the deadline, then allow the
deadline to pass. This guarantees that, after the deadline, no new tokens will ever be minted.

But, in order to check that you only minted one token before the deadline, you need something like a blockchain explorer. So, in this sense, they are not true NFTs, insofar as
the currency symbol itself guarantees that they are unique.

Using Plutus, it is possible to mint true NFTs. If you know the policy script that corresponds to the currency symbol, you can be sure that only one token is in existence
without having to resort to something like a blockchain explorer.

And, thinking about how to do that, there must be a way to prevent there ever being more than one minting transaction for the token in question. Whatever you write in your
policy script, it must only return true for one transaction, so that it is impossible to do the same again in another transaction.

At first, this sounds impossible. Why can't you just run the same transaction again and have validation succeed again? Even considering deadlines, what stops a second transaction
in the same slot from passing validation?

The key here is that we need something unique. Something that can only exist in one transaction and never again. This is an important trick, and it is something to keep in mind.

The idea is to use UTxOs. A UTxO is unique. A UTxO is the output of a transaction and its unique identifier is the transaction ID and its index in the list of outputs from that transaction.

The reason that transactions are unique is a bit subtle. They would not necessarily be unique if it were not for fees. Without fees, you could have a transaction that has
zero inputs and only with outputs without value. Such a transaction would have the exact some hash each time it was run, and therefore the exact same transaction id. But with
fees, such a transaction cannot exist, as you always need an input that provides fees, and the fees can never come from the same UTxO as input.

So, to create an NFT, we are going to provide a specific UTxO as a parameter to the minting policy and, in the policy, we are going to check that the transaction consumes this
UTxO. And, as we have just noted, once that UTxO is consumed, it can never be consumed again.

Example 3 - NFT
~~~~~~~~~~~~~~~

We start with a copy of the previous example, *Signed* and we will call it *NFT*.

So let's turn the signed policy into a true NFT policy.

On-chain
++++++++

First, we will no longer use the public key hash as an input, as if we were a central bank, but will use a UTxO instead. So, what type corresponds to a UTxO?

Let's look in the REPL and remind ourselves about *TxInfo*.

.. code:: haskell

    Prelude Week05.Signed Week05.Free> import Ledger
    Prelude Week05.Signed Ledger Week05.Free> :i TxInfo
    type TxInfo :: *
    data TxInfo
        = TxInfo {txInfoInputs :: [TxInInfo],
                    txInfoInputsFees :: [TxInInfo],
                    txInfoOutputs :: [TxOut],
                    txInfoFee :: Value,
                    txInfoForge :: Value,
                    txInfoDCert :: [Plutus.V1.Ledger.DCert.DCert],
                    txInfoWdrl :: [(Plutus.V1.Ledger.Credential.StakingCredential,
                                    Integer)],
                    txInfoValidRange :: SlotRange,
                    txInfoSignatories :: [PubKeyHash],
                    txInfoData :: [(DatumHash, Datum)],
                    txInfoId :: TxId}

We we are interested in this field:

.. code:: haskell

    txInfoInputs :: [TxInInfo]

Let's look at the type *TxInInfo*

.. code:: haskell
    
    Prelude Week05.Signed Ledger Week05.Free> :i TxInInfo
    type TxInInfo :: *
    data TxInInfo
        = TxInInfo {txInInfoOutRef :: TxOutRef, txInInfoResolved :: TxOut}

We see that it is a record with two fields. The first is of type *TxOutRef*, and this references a UTxO, which is exactly what we need. So, let's use it.

.. code:: haskell

    mkPolicy :: TxOutRef -> ScriptContext -> Bool

Now, we are ready to write the logic. We must check that the script contains the specified UTxO as input. We will delegate this to a helper function. This function, which we
will call *hasUTxO* uses the *any* function, which is a standard Prelude function, but also has a Plutus version, for reasons we have addressed previously.

The *any* function takes a predicate (a function that returns a boolean) and applies it to an input collection of the type *Foldable* (a list, for example), and will
return true if the predicate is true for any of the inputs.

Here, we use the *any* function to see if any of the *txInInfoOutRef*s from the *txInfoInputs* from the *TxInfo* field of the context matches the UTxO for which
we are validating.

For clarity, we will also provide a helper function to get the list of *txInfoInputs*.

.. code:: haskell

    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

So, do we have enough to finish writing our policy? Let's see what we have.

.. code:: haskell

    mkPolicy :: TxOutRef -> ScriptContext -> Bool
    mkPolicy oref ctx = traceIfFalse "UTxO not consumed" hasUTxO
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        hasUTxO :: Bool
        hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

Right now, we have a policy that can only mint or burn once. But, of course, in that single transaction, we can still mint as many tokens as we like.

Now, we think about what we actually want. Maybe we want a policy that allows us to mint just one token for the currency symbol. Or perhaps, we would like to be able
to mint many NFTs at once, each with a different token name.

It's up to us. But, let's say we go with the first option. We just want to mint one token.

So, it makes sense to pass the token name as a parameter.

.. code:: haskell

    mkPolicy :: TxOutRef -> TokenName -> ScriptContext -> Bool

And we need a second condition that checks that we mint just this one specific coin.

.. code:: haskell

    mkPolicy oref tn ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                           traceIfFalse "wrong amount minted" checkMintedAmount

And, of course, we need to implement *checkMintedAmount*.

First of all, we need access to the forged value. We get this from the field *txInfoForge* of *TxInfo*.

How do we check that this forged value is exactly 1 token of the name that we require? There are several approaches, but one is to use the *flattenValue* function which,
we will recall, returns a list of triples of currency symbol, token name and value. We can then check that the output of *flattenValue* is exactly one triple that matches
the symbol, token and value that we expect.

This would look something like this:

.. code:: haskell

    flattenValue (txInfoForge info) == [(cs, tn, 1)]

But we still have a problem to solve - we need to know what the currency symbol is. Given that the currency symbol is a hash of the policy, it seems as if we have a chicken
and egg problem.

As luck would have it, there is a function called *ownCurrencySymbol* which exists to solve exactly this problem.

.. code:: haskell

    flattenValue (txInfoForge info) == [(ownCurrencySymbol ctx, tn, 1)]

As it happens, this won't compile, because *Eq* is not defined for triples in the Plutus Prelude. So, we can work around this with a case statement and some pattern matching.

.. code:: haskell

    case flattenValue (txInfoForge info) of
            [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == tn && amt == 1
            _                -> False

Now, we can complete our policy.

.. code:: haskell

    mkPolicy :: TxOutRef -> TokenName -> ScriptContext -> Bool
    mkPolicy oref tn ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                        traceIfFalse "wrong amount minted" checkMintedAmount
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        hasUTxO :: Bool
        hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

        checkMintedAmount :: Bool
        checkMintedAmount = case flattenValue (txInfoForge info) of
            [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == tn && amt == 1
            _                -> False

And we will update our boilerplate.

.. code:: haskell

    policy :: TxOutRef -> TokenName -> Scripts.MonetaryPolicy
    policy oref tn = mkMonetaryPolicyScript $
        $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMonetaryPolicy $ mkPolicy oref' tn' ||])
        `PlutusTx.applyCode`
        PlutusTx.liftCode oref
        `PlutusTx.applyCode`
        PlutusTx.liftCode tn

    curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
    curSymbol oref tn = scriptCurrencySymbol $ policy oref tn    

That completes the on-chain part.

Off-chain
+++++++++

We need to think about the inputs we need for this transaction.

First, we need a UTxO, and we need to provide one of our own. However, we don't need to pass that in because we can look it up directly.

We only need to provide the token name, so we no longer need a special data type, so we can delete *MintParams* and just use *TokenName*.

.. code:: haskell

    type NFTSchema =
        BlockchainActions
            .\/ Endpoint "mint" TokenName


Now we will write the off-chain *mint* function.

.. code:: haskell

    mint :: TokenName -> Contract w NFTSchema Text ()
    mint tn = do

The first thing to do is to get the list of UTxOs that belong to us.

The *Plutus.Contract* module gives us the *utxoAt* function, which has the signature below, and looks up all the UTxOs at a given address.

.. code:: haskell

    utxoAt :: Address -> Contract w s e Ledger.AddressMap.UtxoMap

An *AddressMap* is a map where the keys are *TxOutRef*s and the values are *TxOutTx*s.

.. code:: haskell

    Prelude Week05.NFT> :i Ledger.AddressMap.UtxoMap
    type Ledger.AddressMap.UtxoMap :: *
    type Ledger.AddressMap.UtxoMap = Data.Map.Internal.Map TxOutRef TxOutTx

If we pass this function our own address then the keys of this map will be the UTxOs that belong to us. It doesn't matter which one of these we pick. So long as we own
at least one UTxO, we are good.

The first step is to find our own address. We know how to find our own public key, and, given this, we can use the function *pubKeyAddress* to get our address.

.. code:: haskell

    pubKeyAddress :: PubKey -> address

Let's get them.

.. code:: haskell

    import qualified Data.Map as Map

    mint :: TokenName -> Contract w NFTSchema Text ()
    mint tn = do
        pk    <- Contract.ownPubKey
        utxos <- utxoAt (pubKeyAddress pk)

We only need one - we don't care which one. We will write a case statement that will either log an error if we have no UTxO available, or will use the first
UTxO in the list continue with the forging code.

The first change is to specify *1* instead of the *mpAmount*, as we want exactly 1 coin minted.

.. code:: haskell

    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let val     = Value.singleton (curSymbol oref tn) tn 1

Secondly, we add the token name argument to the lookups.

.. code:: haskell

    lookups = Constraints.monetaryPolicy $ policy oref tn

Thirdly, we now need an additional constraint which insists that our specific UTxO is consumed.

There's a function for that.

.. code:: haskell

    Prelude Week05.NFT> import Ledger.Constraints
    Prelude Week05.NFT> :t mustSpendPubKeyOutput
    mustSpendPubKeyOutput :: TxOutRef -> TxConstraints i o

How do we combine the constraints of *mustForgeValue* and *mustSpendPubKeyOutput*? *Contraints* don't form a *Monoid*, but they do form a *Semigroup*, and the difference
is just that in *Semigroup* we don't have *mempty*, the neutral element. We can still combine them with the *<>* operator.

.. code:: haskell

    tx = Constraints.mustForgeValue val <> Constraints.mustSpendPubKeyOutput oref

Now, we need to provide a lookup that gives access to where the UTxO *oref* can be found. For that we can use

.. code:: haskell

    Ledger.Constraints.unspentOutputs :: Data.Map.Internal.Map TxOutRef TxOutTx -> ScriptLookups a

So, let's update our lookups.

.. code:: haskell

    lookups = Constraints.monetaryPolicy (policy oref tn) <> Constraints.unspentOutputs utxos

Something we need to do before this script will run is to import the operator *<>* for *Semigroup* from the standard Haskell Prelude, as we have explicitly excluded it from
the *PlutusTx.Prelude* module.

.. code:: haskell

    import Prelude (Semigroup (..))

Let's take a look at the whole function.

.. code:: haskell

    mint :: TokenName -> Contract w NFTSchema Text ()
    mint tn = do
        pk    <- Contract.ownPubKey
        utxos <- utxoAt (pubKeyAddress pk)
        case Map.keys utxos of
            []       -> Contract.logError @String "no utxo found"
            oref : _ -> do
                let val     = Value.singleton (curSymbol oref tn) tn 1
                    lookups = Constraints.monetaryPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
                    tx      = Constraints.mustForgeValue val <> Constraints.mustSpendPubKeyOutput oref
                ledgerTx <- submitTxConstraintsWith @Void lookups tx
                void $ awaitTxConfirmed $ txId ledgerTx
                Contract.logInfo @String $ printf "forged %s" (show val)

For the test script.

.. code:: haskell

    test :: IO ()
    test = runEmulatorTraceIO $ do
        let tn = "ABC"
        h1 <- activateContractWallet (Wallet 1) endpoints
        h2 <- activateContractWallet (Wallet 2) endpoints
        callEndpoint @"mint" h1 tn
        callEndpoint @"mint" h2 tn
        void $ Emulator.waitNSlots 1

Let's test.

.. code:: haskell

    Prelude Week05.Signed Ledger Plutus.Contract Ledger.Constraints Week05.Free> Week05.NFT.test
    Slot 00000: TxnValidate af5e6d25b5ecb26185289a03d50786b7ac4425b21849143ed7e18bcd70dc4db8
    Slot 00000: SlotAdd Slot 1
    Slot 00001: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
    Contract instance started
    Slot 00001: 00000000-0000-4000-8000-000000000001 {Contract instance for wallet 2}:
    Contract instance started
    Slot 00001: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
    Receive endpoint call: Object (fromList [("tag",String "mint"),("value",Object (fromList [("unEndpointValue",Object (fromList [("unTokenName",String "ABC")]))]))])
    Slot 00001: W1: TxSubmit: 691a5c0725ac09f79c8c45c899d732d26460d18c4c18167be71d55319bcd5669
    Slot 00001: 00000000-0000-4000-8000-000000000001 {Contract instance for wallet 2}:
    Receive endpoint call: Object (fromList [("tag",String "mint"),("value",Object (fromList [("unEndpointValue",Object (fromList [("unTokenName",String "ABC")]))]))])
    Slot 00001: W2: TxSubmit: e53519b17bf7d11a148ce17ac0305330f138a684530ba08b1c57f714672b8c68
    Slot 00001: TxnValidate e53519b17bf7d11a148ce17ac0305330f138a684530ba08b1c57f714672b8c68
    Slot 00001: TxnValidate 691a5c0725ac09f79c8c45c899d732d26460d18c4c18167be71d55319bcd5669
    Slot 00001: SlotAdd Slot 2
    Slot 00002: *** CONTRACT LOG: "forged Value (Map [(9d969e597d45fcd1732ce255e12a97599e883f924b4565fc3a2407bc08d34524,Map [(\"ABC\",1)])])"
    Slot 00002: *** CONTRACT LOG: "forged Value (Map [(913f220c3b1ba49531bae2fedd9edb138a8b360e7e605bfcf4ff3f2045433069,Map [(\"ABC\",1)])])"
    Slot 00002: SlotAdd Slot 3
    Final balances
    Wallet 1: 
        {9d969e597d45fcd1732ce255e12a97599e883f924b4565fc3a2407bc08d34524, "ABC"}: 1
        {, ""}: 99999990
    Wallet 2: 
        {913f220c3b1ba49531bae2fedd9edb138a8b360e7e605bfcf4ff3f2045433069, "ABC"}: 1
        {, ""}: 99999990
    ...
    Wallet 10: 
        {, ""}: 100000000

And now we have minted some NFTs.
























    










 
 



    




