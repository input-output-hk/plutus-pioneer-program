Week 10 - Uniswap
=================

.. note::
      These is a written version of `Lecture
      #10 <https://youtu.be/Dg36h9YPMz4>`__.

      In this lecture we look at an implementation of Uniswap in Plutus.

      This is the last lecture in the Plutus Pioneer Program. However, there will be a special lecture once it is possible to deploy contracts to the testnet.

In this lecture we won't be introducing any new topics or concepts. Instead we will do an end-to-end walk through of a demo that Lars wrote some months ago that 
clones the very popular Uniswap contract from Ethereum.

The one new thing we will look at following several requests is how you can query the endpoints created by the PAB with Curl commands just from the console.

What is Uniswap
---------------

So for those of you who haven't heard of Uniswap, what is Uniswap?

Uniswap is a so-called DeFi, or decentralized finance application, that allows swapping of tokens without any central authority. In 
the case of Ethereum it's ERC20 tokens.

So you don't need a centralized exchange, the traditional way to exchange tokens or other crypto assets. Instead everything is governed by 
smart contracts and works fully automatically on the blockchain.

Another interesting feature of Uniswap is that it doesn't discover prices the usual way with the so-called order book, but uses a different
automatic price discovery system. The idea is that people can create so-called liquidity pools. 

If they want other users to be able to swap two different tokens, then somebody can create a liquidity pool and put a certain amount of those two tokens 
in this liquidity pool, and in return the creator of the pool will receive so-called liquidity tokens that are specific to this one pool. 

Other users can use that pool to swap. They take some amount of one of the tokens out in exchange for putting an amount of the other token back in.

Additionally, people can also add liquidity to the pool and receive liquidity tokens, or they can also burn liquidity tokens in exchange for tokens from the pool.

And all these features are also implemented in the version of Uniswap that works on Cardano that we're going to look at now.

.. figure:: img/pic__00149.png

So let's look at the various operations that are available in turn.

It all starts by somebody setting up the whole system. So some organization or entity that wants to offer this Uniswap service. 

It starts with a transaction that creates a UTxO at this script address, here we call that *factory* for Uniswap factory. It contains an NFT that identifies the factory, 
the same trick that we have used a couple of times before, and as datum, it will contain the list of all liquidity pools.

So in the beginning, when the factory is just being created, that list will be empty.

Now let's assume that one user, Alice wants to create a liquidity pool for tokens A and B. A pool that allows others to swap A against B or B against A.

.. figure:: img/pic__00150.png

She has to provide some initial liquidity for the pool. So she needs some amount of token A and some amount of token B, let's say she has 1,000A and 2000B.

It's important to note here that the ratio between A and B reflects Alice's belief in the relative value of the tokens. So if she wants to set up a pool with 
1000A and 2000B, then she believes that one A has the same value as two Bs.

In order to create the liquidity pool, she will create a transaction with two inputs and three outputs.

.. figure:: img/pic__00150.png

The two inputs will be the liquidity she wants to provide; the 1000A and 2000B and the Uniswap factory invoked with the create redeemer. The three outputs 
will be the newly-created pool.

We call it *Pool AB* here to indicate that it contains tokens AB, which will contain the liquidity that Alice provided; the 1000A and the 2000B and a freshly-minted 
token that identifies this pool, an NFT, called *AB NFT* here.

The datum of the pool, the 1415, will be the amount of liquidity tokens that Alice receives in return for setting up this pool and providing the liquidity. #

If you wonder about the number, that is the square root of the product of 1000 and 2000, so that's how the initial amount of liquidity tokens is calculated. It 
doesn't really matter, you could scale it arbitrarily, but that's the way Uniswap does it.

The second output is the Uniswap factory again, with the same NFT as before that identifies it. And now the datum has been updated. So in this list that was 
empty before, the list of all liquidity pools, there is now an entry for the newly-created AB pool.

Finally, there's a third output for Alice, where she receives the freshly-minted liquidity tokens, called *AB* here to indicate that they belong to the pool AB.

Now that the liquidity pool has been set up, other users can use it to swap.

.. figure:: img/pic__00151.png
