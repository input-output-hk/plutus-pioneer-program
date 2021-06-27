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

So let's assume that Bob wants to swap 100A against B. What will Bob do?

He will create a transaction that has two inputs and two outputs. The two inputs are the 100A he wants to swap, and the pool with the swap redeemer. The outputs 
are the Bs he gets in return.

In this example, that would be 181B and the updated pool. So the pool now has the additional 100A that Bob provided. So now it's 1,100A, and it has 181B fewer than before.

It still, of course, has the NFT that identifies the pool and the datum hasn't changed because the amount of liquidity tokens that have been minted hasn't changed.

Now, of course, the question is, where does this 181 come from? This is this ingenious idea, how price discovery works in Uniswap.

So the rule is roughly that the product of the amounts of the two tokens must never decrease. Initially we have 1000 As and 2000 Bs and the product is 2 million.

If you do the calculation, then you will see that after the swap 1100*1819 will be slightly larger than 2 million.

If you think about it or try a couple of examples by yourself, then you will see that in principle, you will always pay this ratio of the As and Bs in the pool, at least if you swap small amounts.

So originally the ratio from A to B was 1:2, 1000:2000. 100 is relatively small in comparison to the 1000 liquidity, so Bob should roughly get 200B, but he does get less
and there are two reasons for that.

One is that the amount of tokens in the liquidity pool is never allowed to go to zero. And the more of one sort you take out, the more expensive it gets - 
the less you get in return. So 100 depletes the pool a bit of As, so Bob doesn't get the full factor 2 out, he gets a little bit less out. That's exactly how this product formula works.

This also makes it ingenious, because it automatically accounts for supply and demand. If the next person also wants to swap 100A, they would get even less out.

The idea is if a lot of people want to put A in and want to get B in return, that means the demand for B is high. And that means the price of B in relation to A 
should rise. And that is exactly what's happening.

So the more people do a swap in this direction, put A in and get B out, the less of the gap because the price of B rises. If there were swaps in the other direction, 
you would have the opposite effect.

If there's an equal amount of swaps from A to B and B to A, then this ratio between the two amounts would stay roughly the same.

There's an additional reason why Bob doesn't get the full 200 that he might expect, and that is fees.

We want to incentivize Alice to set up the pool in the first place. She won't just do that for fun, she wants to profit from it, so she wants to earn on swaps that people make.

The original product formula is modified a bit to insist that the product doesn't only not decrease, but that it increases by a certain amount, a certain percentage, 
depending on how much people swap. That's 3% in this example of the 100A that Bob swaps, and it would be the same if you swap B instead.

This is basically added on top of this product, so anytime somebody swaps, not only does the product not decrease, it actually increases. And the more people swap, the more it increases.

The idea is that if Alice now would close the pool by burning her liquidity tokens, she gets all the remaining tokens in the pool and the product 
would be higher than what she originally put in.

So that's her incentive to set up the pool in the first place. 

The next operation we look at is the add operation where somebody supplies the pool with additional liquidity.

.. figure:: img/pic__00152.png
