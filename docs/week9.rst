Week 09 - Marlowe
=================

.. note::
    This is a written version of `Lecture
    #9 <https://youtu.be/-RpCqHuxfQQ>`__.

    In this lecture we cover Marlowe - a special-purpose language for financial contracts on Cardano.

Overview
--------

In the previous lectures we have learnt about all the important ingredients for writing a Plutus application.

We have first looked at the extended UTxO model - the accounting model that Cardano uses - and the additions that Plutus brings to it.

Then we have talked about on-chain validation, minting policies, writing off-chain code, we have seen how to deploy smart contracts and also how to test them.

Plutus is a very powerful language. So powerful, in fact, that you can implement other languages on top of it - you can write an interpreter in Plutus for other languages.

One such language is Marlowe. Marlowe is a Domain Specific Language (DSL) for smart contracts.

For this lecture, Professor Simon Thomson, a very prominent figure in the Haskell community who leads the Marlowe team, and his colleague Alex Nemish will give guest lectures to tell us a bit about Marlowe.

Afterwards we will look at the Marlowe playground and play with a simple smart contract.

Lecture by Prof. Simon Thomson
------------------------------

Marlowe is a special-purpose language for writing financial contracts on Cardano.

.. figure:: img/pic__00005.png

Why do we build DSLs? 
~~~~~~~~~~~~~~~~~~~~~

One reason is that we want to build languages that are closer to the language of the user and not so much the language of the system. They are 
designed to be in the specific domain of the application. A financial language will talk about payments, for example.

When we write a DSL, we get some advantages. We can write down things in that domain, but we can't perhaps write as much as we could in a general purpose language. And,
if we do work in this more specialised context, we have the advantage of being able to give people better feedback and better error messages. We can also give more
guarantees on program behaviour. That's one of the things that will be stressed in this lecture.

.. figure:: img/pic__00006.png

What kind of assurance can we give?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can give two kinds of assurance. We can make sure that contracts do what they are supposed to do, but we can also make sure that they don't do what they shouldn't. We 
will see both aspects of that as we go along.

We've designed the language to be a simple as possible and the implementation reflects that, and we'll talk a bit about that later on. Contracts are nice and readable, and also
we can easily simulate them, so we can present to users a very clear picture of how their contract in Marlowe will behave.

In fact, we can do more than that. Because they are particularly restricted, we can explore every possible behavior path that a contract can take, before it is executed. So, we 
can give complete guarantees about how a contract will behave, not just on one or two tests, but on every possibly execution sequence.

It's also more straightforward to write mathematical proofs of various kinds of safety, so that is the strongest criteria that we can hit in this kind of world; a mathematical 
proof that the system will do certain things and won't do others.

What does a financial contract do?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's start by looking at what a financial contract can do. 

A contract can accept payments from participants in the contract.

Depending on choices made by one the participants, it can evolve in different directions. 

.. figure:: img/pic__00011.png

It can make decisions based on external information such as the information coming from a stock exchange. So, information coming from an oracle can determine the future behaviour
of a contract.
 
A contract can also make payments out. If money has been deposited in the contract, that money can be deposited out to participants.

So we have flows of money and choices according to external factors.

One final thing that we have is that the roles in a contract are things that themselves can be owned. We represent that in Marlowe by minting tokens that 
represent those roles. That means that we can use those tokens as evidence that somebody is meant to be playing a role. They are a form of security that a person 
submitting a transaction is allowed to submit that transaction, but also it means that these roles are tradable. A role could be traded by another person or another contract.

Language design
~~~~~~~~~~~~~~~

Now let's think about how to design a language based on these ingredients.

.. figure:: img/pic__00012.png

When we design a language of contracts, what we are really doing is designing a programming language. A smart contract is just a program running on a blockchain.

A contract could, in principle, run forever. And also, more subtly, it could get stuck waiting for an input forever.

It could terminate while holding assets, locking them up forever.

So there's a whole lot of security issues that a program might have.

Designed for safety
+++++++++++++++++++

What we chose to do was to design for safety.

Contracts are finite
____________________

Firstly, contracts are designed to be finite. Their life will be finite, there is no recursion or looping in Marlowe. We will come back to that a bit later on when 
we talk about Marlowe being embedded in other languages.

Contracts will terminate
________________________

We can be sure that contracts will terminate. We do that by putting timeouts on every external action. Every choice, every deposit of money into the contract comes with
a deadline. Marlowe contracts cannot wait forever for somebody to make a choice or for an action to happen. If you hit the timeout then an alternative course is taken.

No assets retained on close
___________________________

We've designed the semantics of the language so that when a contract reaches its close, at the end of its lifetime, any money left in the contract will be 
refunded to participants.

Conservation of value
_____________________

Conservation of value is something that we get for free from the underlying blockchain. The blockchain guarantees that we can't double spend and because we are using 
the transaction mechanisms of the underlying blockchain, we can be sure that we are getting conservation of value.

So this is giving us a lot of guarantees out of the box. These are not guarantees that you get from Plutus contracts in general. A Plutus contract could go on forever, 
it need not terminate and it could terminate while holding a whole collection of assets which then become unreachable.

The Marlowe Language
~~~~~~~~~~~~~~~~~~~~

So what does the language look like? Let's cut to the chase.

.. figure:: img/pic__00013.png

Marlowe, at heart, is represented as a Haskell datatype.

.. code:: haskell

    data Contract = Close
    | Pay Party Payee Value Contract
    | If Observation Contract Contract
    | When [Case Action Contract] Timeout Contract
    | Let ValueId Value Contract
    | Assert Observation Contract
    deriving (Eq,Ord,Show,Read,Generic,Pretty)

We have a *Pay* construct. In that a *Party* in the contract makes a payment to a *Payee* of a particular *Value*, and then the contract continues with what we call the 
continuation contract.

.. code:: haskell

    Pay Party Payee Value Contract
    
We can go in two separate directions. We can observe *If* a particular *Observation* is true or not. If the observation is true we follow the first *Contract*, if it is 
false we follow the second *Contract*.

.. code:: haskell

    If Observation Contract Contract

The most complex construct in Marlowe is the *When* construct. It takes three arguments. The first of those is a list of *Contract*/*Action* pairs - a list of *Case*\s.

.. code:: haskell

    When [Case Action Contract] Timeout Contract

What the *When* construct does is wait for one of a number of *Action*\s. When one of those *Action*\s happens, it performs the corresponding *Contract*. For example, it
could be waiting for a deposit. If we have a case where the first part of the pair is a deposit, then we execute the corresponding second part of the pair. Similarly with 
making a choice or with getting a value from an oracle.

Here we are waiting for external actions and, of course, the contract can't make those actions happen. A contract can't force somebody to make a choice. It can't force
somebody to make a deposit. But what we can do is say that if none of these actions takes place then we will hit the *Timeout*, and when we hit the *Timeout*, we will perform 
the *Contract* represented by the final argument to the *When* construct.

So, we can guarantee that something will happen in the *When* construct, either by one of the actions triggering a successive contract, or we hit the timeout and go to that 
continuation.

Finally we have the *Close* construct which has the semantics defined so that nothing is retained when we close.

That is the Marlowe language, and we will see that we can use these to construct Marlowe contracts in a variety of ways.

The Marlowe Product
~~~~~~~~~~~~~~~~~~~

So that is the language. What is the Marlowe product itself?

We have a suite of things. First we'll look at the overall vision for Marlowe and then look at where we are in terms of fulfilling that vision.

.. figure:: img/pic__00020.png

We have a prototype for Marlowe Run. That is the system through which an end user will interact with contracts running on the Cardano blockchain. You can think of Marlowe 
Run as the Marlowe dApp. It's the things that allows Marlowe contracts to be executed.

We're also building a market where contracts can be uploaded, downloaded, and where we can provide various kinds of assurance about those contracts.

We allow contracts to be simulated interactively and we call that Marlowe Play. We allow contracts to be built in various different ways and we call that Marlowe Build. In 
fact fact what we've done at the moment is bundle those two - Marlowe Play and Build - into what we call the Marlowe Playground.

So as things stand at the moment you can use the Marlowe Playground to simulate and construct Marlowe contracts we're in the process of redesigning the user experience
based on what we've done with Marlowe Run.

What we're releasing very shortly is the prototype of Marlowe Run and this is the prototype of how end users will interact with Marlowe on the blockchain. Our 
intention is that we'll have all these products available running on the Cardano blockchain when we have the full support for this which will involve having the
Plutus Application Backend and the wallet back end and so on working as they should.

Demonstration
~~~~~~~~~~~~~

We'll now look at a demo of what we have in Marlowe Run to give you a sense of what we can do at the moment in terms of giving users the 
experience that they will have when Marlowe is running on blockchain. This will be the app that is going to provide that experience.

At the moment it's running locally but in a few weeks' time we will be releasing a version that runs in a distributed fashion on the simulated blockchain.
Then, as we go into the end of the year we expect to have it running for real on the Cardano blockchain itself.

You can find the Marlowe Playground at

.. code::

    https://staging.marlowe-dash.iohkdev.io/

.. figure:: img/pic__00023.png

Marlowe run runs in the browser and what it does is provide the end user interaction with contracts running on the blockchain.

For the moment we're simulating that blockchain inside the browser but eventually this will be the tool you'll use to run contracts for real on Cardano.

To interact with the contract your wallet needs to be involved to control your your signature and to control your assets, so we link up Marlowe to run with
a wallet. Let's link it up with Shruti's wallet. You can do this by creating a demo wallet, or by selecting an existing wallet.

.. figure:: img/pic__00024.png

In this window we see the world from Shruti's perspective. Let's open up another window and link that window to the world from Charles's perspective.

.. figure:: img/pic__00028.png

At the moment neither of them has any contracts running. They have a blank space, but let's start a contract up. Let's set up a zero coupon bond which is a fancy name
for a loan. You can do this by clicking *Create* and selecting the *Zero Coupon Bond* option.

Let's suppose that Shruti is making a loan to Charles. She's the investor he's the issuer of the bond.

.. figure:: img/pic__00034.png

Charles wants to borrow one Ada from Shruti and he's promised to pay back 1.1 Ada. So we've said who the issuer and investor are we said what the price and
the eventual value will be and we're now going to create the contract. In order to do that we have to make a payment of 30 lovelace to get the contract started.

.. figure:: img/pic__00035.png

So let's pay. We are asked to approve and the payment goes through. You can see now in Shruti's Marlowe Run we've got the Zero Coupon Bond running, but also,
if you look at Charles's view of the world, it's running there too for him.

.. figure:: img/pic__00037.png

We're at the first step. If we click through on Charles's contract, it's saying that it's waiting for something from the investor, who is Shruti. 

.. figure:: img/pic__00038.png

So let's see what's happening in her view.

.. figure:: img/pic__00039.png

She's being asked to make a deposit so let's click on that to make the deposit.

.. figure:: img/pic__00040.png

And click to confirm with a fee of 10 lovelace.

Then you can see her view has changed now she's waiting for the issuer to pay her back.

We look in Charles's view, which is incidentally the mobile view, of Marlowe Run, and he's asked to pay his 1 Ada.

.. figure:: img/pic__00041.png

Let's make him do that now. He'll also have to pay a 10 lovelace transaction fee. 

.. figure:: img/pic__00043.png

Let's make that deposit.

.. figure:: img/pic__00045.png

And you see now from both their perspectives that loan is completed you can see the history of what's gone on. You can see, at particular points, the
balances that the contract holds.

If we close that and select *History*, we can see the history of all the contracts that Shruti has taken part in.

.. figure:: img/pic__00046.png

That pretty much covers the basics of what you get from Marlowe Run. It's an intuitive interface to a contract running on the blockchain.
You see that each participant in the contract gets their view of the contract in real time, updated from what is, in this case in the browser, but
eventually what's on the blockchain.


