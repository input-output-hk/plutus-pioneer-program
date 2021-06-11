Week 07 - State Machines
========================

.. note::
    This is a written version of `Lecture
    #7 <https://www.youtube.com/watch?v=oJupInqvJUI>`__.

    It covers commit schemes and state machines.

Overview
--------

In this lecture we will look at state machines. State machines can be very useful to shorter and more concise contracts, both on-chain and off-chain. There is higher level support for state machines in the Plutus libraries that builds on top of the lower level mechanisms we have seen so far.

As a running example, we are going to implement a little game, played between Alice and Bob. It's a bit like Rock, Paper, Scissors, but even simpler, because there are 
only two options.

Alice and Bob both have two options, they can either play 0 or 1.

.. figure:: img/week07__00000.png

If there were to play this game while being physically in the same room, they would make their moves at the same time. There would be one gesture for 0 and one
gesture for 1, they would raise their hands simultaneously, and, depending on what they play, one of them wins.

If they both play the same number, Alice wins. If they play different numbers, Bob wins.

.. figure:: img/week07__00001.png

Now let's imagine that Alice and Bob can't meet in person but that they still want to play the game. So, they decide to play it via mail - email or snail mail, it doesn't
matter. How would that work?

Alice could send her move to Bob.

.. figure:: img/week07__00004.png

This, however, gives a very unfair advantage to Bob, because now he opens Alice's mail, see that she has played 0, and he can simply reply with 1, and he wins.

.. figure:: img/week07__00003.png

And, if Alice plays 1, Bob can simply respond with 1. So Bob always wins, at least if he is unfair.

.. figure:: img/week07__00005.png

What can we do about that? 

There's a very clever trick which is often used in cryptographic protocols, and that is commit schemes. The idea is that Alice doesn't reveal her choice to Bob, but she commits to it, so that she cannot later change her mind.

One way to make that work is using hash functions.

Hashes are all over the place in the blockchain world. We have seen that script addresses are just the hash of the Plutus code script, and we have seen lots of examples of
public key hashes.

A hash function is a one-way function. Given a hash, tt is difficult, or impossible, to reconstruct the original byte string that was hashed.

So, one way we could try to make this work is that, instead of Alice sending her choice to Bob, she instead sends the hash of her choice.

.. figure:: img/week07__00006.png

Bob then sees this cryptic byte string and he has no idea whether Alice picked 0 or 1.

Bob then replies with his move, picking, for example 0. There is no need for him to use a hash, he can just send his response in clear text. 

Now, Alice would have won. But perhaps Bob doesn't believe her. So there is one additional step that Alice has to take.

Alice has to send her actual choice to Bob in clear text. Bob then has to check that the hash of her choice is indeed the same as the hash Alice sent earlier.

.. figure:: img/week07__00007.png

If it is, then he knows sees that Alice is not lying and that indeed he lost. If it does not match, then he knows that Alice is cheating and he would win.

This all sounds promising, but there is one big problem with it.

In this game there are only two choices, 0 and 1. Which means that there are only two possible hashes. They may look very cryptic to Bob the first time they play, 
but before long he will notice that he always sees one of only two possible hashes, and then he can know which choice Alice made.

This is almost as bad as if Alice had just sent her choice in clear text.

What we can do about this is that, instead of sending the hash of her choice, she instead first selects an arbitrary byte string and then hashes the
concatenated of this byte string and her choice. The arbitrary byte string that Alice chooses is called a *nonce* - a number to be used just once.

.. figure:: img/week07__00008.png














