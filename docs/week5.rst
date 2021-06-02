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







    




