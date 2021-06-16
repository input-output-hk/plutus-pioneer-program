Week 08 - Property Based Testing
================================

.. note::
    This is a written version of `Lecture
    #8 <https://youtu.be/JMRwkMgaBOg>`__.

    In this lecture we cover another state machine example, automatic testing using emulator traces, optics, and property-based testing.

    This week we were using Plutus commit 530cc134364ae186f39fb2b54239fb7c5e2986e9, the same commit as we used in the last lecture.

Introduction
------------

In the last lecture we looked at state machines, and saw how they often allow us to write much less code to express the logic of a smart contract, partly because  
there is a lot of sharing between on-chain and off-chain code and partly because a lot of boilerplate is encapsulated in the state machine machinery.

In this lecture we will see another example using a state machine, because the concept is very important. We will also take a look at testing. 
First we will look at the code, then we will explore various ways to go about testing.

The example we will use is a contract that allows somebody to sell tokens. The idea is that someone call lock some tokens in a contract, set a price, and then 
other people can buy them.

To begin, the seller starts with an NFT. It can be an arbitrary NFT and it will just be used, as before, to identify the correct UTxO that contains the contract state.

The first step is to lock the NFT at the script address of the smart contract that we are about to write. We'll call that contract *TS* for Token Sale. As a datum, we will 
use a simple integer, which will represent the price of the token we are selling, and this will start off as zero.

.. figure:: img/week08__00001.png

There will be several operations that the seller can do. One of those will be setting the price to a different value. In order to do that the seller will submit
a transaction which has the current UTxO as input and the updated UTxO as output, where the datum has been changed to a different price.

.. figure:: img/week08__00002.png


