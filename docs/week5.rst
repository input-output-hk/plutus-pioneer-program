Week 05
=======

.. note::
      These is a written version of `Lecture
      #5 <https://youtu.be/6VbhY162GQA>`__.

      In this lecture we learn about native tokens, minting policies and NFTs.


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

The first thing to note is that each native token, including Ada, is identified by two pieces of data.




