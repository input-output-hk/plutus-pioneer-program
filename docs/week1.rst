Week 01 - Introduction
======================

.. note::
   This is a written version of `Lecture
   #1 <https://youtu.be/IEn6jUo-0vU>`__.

   It covers an introduction to Plutus, the (E)UTxO model (and how it
   compares to other models), and concludes with an example English Auction
   managed with a Plutus smart contract running on the Plutus Playground.

Welcome
-------

Learning Plutus isn't easy, and there are a number of reasons for that.

1. Plutus uses the (E)UTxO model. This is different and less intuitive
   than the Ethereum method for creating smart contracts. It has a lot
   of advantages, but it requires a new way of thinking about smart
   contracts. And that's before we even start with the language itself.
2. Plutus is new and still under rapid development.
3. Tooling is not ideal, yet. So, experienced Haskell developers will
   notice that the experience with Plutus is not as pleasant, for
   example, when trying to access documentation or get syntax hints from
   the REPL. It can also be a challenge to build Plutus in the first
   place. The easiest way currently is to use Nix. The Plutus team is
   working on providing a Docker image, which will help.
4. Plutus is Haskell, more or less, which can have a tough learning
   curve for those coming from an imperative programming background.
5. Plutus is brand new and this means that there are not many help
   resources available, such as StackOverflow posts.

The (E)UTxO Model
-----------------

Overview
~~~~~~~~

One of the most important things you need to understand in order to
write Plutus smart contracts is the accounting model that Cardano uses,
the Extended Unspent Transaction Output model.

The UTxO model, without the (E) is the one that was introduced by
Bitcoin, but there are other models. Ethereum for example, uses the
so-called account-based model, which is what you are used to from normal
banking, where everybody has an account, each account has a balance
and if you transfer money from one account to another then the balances
get updated accordingly.

That is not how the UTxO model works.

Unspent transaction outputs are exactly what the name says. They are
transaction outputs from previous transactions that have happened on the
blockchain and have not yet been spent. Let's look at an example where
we have two such UTxOs.

.. figure:: img/pic__00000.png

Alice wants to send 10 ADA to Bob, so she needs to create a transaction. 

.. note::
    A transaction is something that contains an arbitrary number of inputs and
    an arbitrary number of outputs. The effect of a transaction is to consume 
    inputs and produce new outputs.

The important thing is that you can only ever use complete UTxOs as
input. Alice cannot simply split her existing 100 ADA into a 90 and a
10, she has to use the full 100 ADA as the input to a transaction.

.. figure:: img/2.png
   :alt: Image 2

Once consumed by the transaction, Alice's input is no longer a UTxO (an unspent transaction). It will
have been spent as an input to Tx 1. So, she needs to create outputs for her transaction.

She wants to pay 10 ADA to Bob, so one output will be 10 ADA (to Bob).
She then wants her change back, so she creates a second output of 90
ADA (to herself). The full UTxO of 100 ADA has been spent, with Bob
receiving a new transaction of 10 ADA, and Alice receiving the change
of 90 ADA.

.. figure:: img/3.png
   :alt: Image 3

In any transaction, the sum of the output values must match the sum of
the input values. Although, strictly speaking, this is not true. There
are two exceptions.

1. Transaction fees. In a real blockchain, you have to pay fees for each
   transactions.
2. Native Tokens. It's possible for transactions to create new tokens,
   or to burn tokens, in which case the inputs will be lower or higher
   than the outputs, depending on the scenario.


Let's take a look at a slightly more complicated example.

Alice and Bob want to transfer 55 ADA each to Charlie. Alice has no
choice, as she only has one UTxO. Bob also has no choice as neither of
his two UTxOs is large enough to cover the 55 ADA he wishes to send to
Charlie. Bob will have to use both his UTxOs as input.

.. figure:: img/4.png
   :alt: Image 4

When Is Spending Allowed?
~~~~~~~~~~~~~~~~~~~~~~~~~

Obviously it wouldn't be a good idea if any transaction could spend
arbitrary UTxOs. If that was the case then Bob could spend Alice's money
without her consent.

The way it works is by adding signatures to transactions.

In transaction 1, Alice's signature has to be added to the transaction.
In transaction 2, both Alice and Bob need to sign the transaction.

Everything explained so far is just about the UTxO model, not the
(E)UTxO model.

The extended part comes in when we talk about smart contracts, so in
order to understand that, let's concentrate on the consumption of
Alice's UTxO of 100 ADA by Tx 1.

.. figure:: img/5.png
   :alt: Image 5

In the UTxO model, the validation that decides whether the transaction
that this input belongs to is allowed to consume the UTxO, relies on
digital signatures. In this case, that means that Alice has to sign the
transaction in order for the consumption of the UTxO to be valid.

The idea of the (E)UTxO model is to make this more general.

Instead of having just one condition, namely that the appropriate
signature is present in the transaction, we replace this with arbitrary
logic. This is where Plutus comes in.

Instead of just having an address that corresponds to a public key that
can be verified by a signature that is added to the transaction, we have
more general logic, not based on public keys or the hashes of public
keys, but instead arbitrary logic which decides under which conditions a
particular UTxO can be spent by particular transaction.

The input will justify that it is allowed to consume this output with
some arbitrary piece of logic that is called the Redeemer.

.. figure:: img/6.png
   :alt: Image 6

What exactly does that mean?

It is important to consider the context that the script has. There are
several options.

Script Context
~~~~~~~~~~~~~~

The Bitcoin approach
^^^^^^^^^^^^^^^^^^^^

One option is that all the script sees is the Redeemer. In this case,
the Redeemer contains all the logic necessary to verify the transaction.
This is, incidentally, what Bitcoin does. In Bitcoin, there are smart
contracts, but they are just not very smart. They are called Bitcoin
Script, which works exactly like this. There is a script on the UTxO
side and a redeemer on the input side, and the script gets the redeemer
and uses it to determine if it is ok to consume the UTxO or not.

But this is not the only option. We can decide to give more information
to the script.

The Ethereum approach
^^^^^^^^^^^^^^^^^^^^^

Ethereum uses a different concept. In Ethereum, the script can see
everything - the whole blockchain - the opposite extreme of Bitcoin. In
Bitcoin, the script has very little context, all it can see is the
redeemer. In Ethereum, the Solidity scripts can see the complete state
of the blockchain.

This makes Ethereum scripts more powerful, but it also comes with
problems. Because the scripts are so powerful it is difficult to predict
what a given script will do and that opens the door to all sorts of
security issues and dangers. It is very hard for the developers of an
Ethereum smart contract to predict everything that can happen.

The Cardano approach
^^^^^^^^^^^^^^^^^^^^

What Cardano does is something in the middle.

In Plutus, the script cannot see the whole blockchain, but it can see
the whole transaction that is being validated. In contrast to Bitcoin,
it can't see only the redeemer of the one input, but it can also see all
the inputs and outputs of the transaction, and the transaction itself.
The Plutus script can use this information to decide whether it is ok to
consume the output.

There is one last ingredient that Plutus scripts need in order to be as
powerful and expressive as Ethereum scripts. That is the so-called
Datum. That is a piece of data that can be associated with a UTxO along
with the UTxO value.

.. figure:: img/7.png
   :alt: Image 7

With this it is possible to prove mathematically that Plutus is at least
as powerful as the Ethereum model - any logic you can express in
Ethereum you can also it can express using the (E)UTxO model.

But it also has a lot of advantages compared to the Ethereum model. For
example, in Plutus, it is possible to check whether a transaction will
validate in your wallet, before you ever send it to the chain.

Things can still go wrong with off-chain validation, however. For
example in the situation where you submit a transaction that has been
validated in the wallet but gets rejected when it attempts to consume an
output on-chain that has already been consumed by another transaction.

In this case, your transaction will fail without you having to pay any
fees.

But if all the inputs are still there that your transaction expects,
then you can be sure that the transaction will validate and will have
the predicted effect.

This is not the case with Ethereum. In Ethereum, the time between you
constructing a transaction and it being incorporated into the
blockchain, a lot of stuff can happen concurrently, and that is
unpredictable and can have unpredictable effects on what will happen
when your script finally executes.

In Ethereum it is always possible that you have to pay gas fees for a
transaction even if the transaction eventually fails with an error. And
that is guaranteed to never happen with Cardano.

In addition to that, it is also easier to analyse a Plutus script and to
check, or even prove, that it is secure, because you don't have to
consider the whole state of the blockchain, which is unknowable. You can
concentrate on this context that just consists of the spending
transaction. So you have a much more limited scope and that makes it
much easier to understand what a script is actually doing and what can
possibly go wrong.

The (E)UTxO model is not tied to a particular programming language. What
we have is Plutus, which is Haskell, but in principal you could use the
same model with a completely different programming language, and we
intend to write compilers for other programming languages to Plutus
Script which is the "assembly" language underlying Plutus.

Running an example auction contract on a local Playground
---------------------------------------------------------

Rather than start the traditional way, i.e. starting very simple and
doing a crash course on Haskell, followed by some simple Plutus
contracts and slowly add more complicated stuff, it will be more
interesting, especially for the first lecture, to showcase a more
interesting contract and demonstrate what Plutus can do. We can then use
that to look at certain concepts in more detail.

Plutus Setup
~~~~~~~~~~~~

Before compiling the sample contract code, we need to setup Plutus. It
is advisable to set up a Nix shell from the main Plutus repository at
which can also be used to compile the example contracts.

`There are detailed notes on how to do this
here <https://www.evernote.com/shard/s426/client/snv?noteGuid=b34acc67-c94b-fc64-9350-398a8f6fc6ec&noteKey=7e6b84c9501e9949eef2cadf6e35eaff&sn=https%3A%2F%2Fwww.evernote.com%2Fshard%2Fs426%2Fsh%2Fb34acc67-c94b-fc64-9350-398a8f6fc6ec%2F7e6b84c9501e9949eef2cadf6e35eaff&title=Installation>`__.

This will setup your environment with the dependencies necessary to
compile the sample contracts.

Once you are inside the Nix shell, you can start the Plutus client and
server from the cloned Plutus repository.

The lecture videos were recorded at various times and the Plutus code
that goes along with them was compiled against specific commits of the
Plutus main branch. You can find the commit tag in the cabal.project
file.

Server
^^^^^^

.. code:: bash

      cd /path/to/plutus/repo/plutus-playground-client
      plutus-playground-server

Client
^^^^^^

.. code:: bash

      cd /path/to/plutus/repo/plutus-playground-client
      npm run start

To check that everything is in order, you can then compile the code for
Week 01. This is not necessary to run the code in the playground, as the
playground can compile the code itself.

.. code:: bash

      cd /path/to/plutus-pioneer-program/repo/code/week01
      cabal build all

The English Auction contract
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The code for the English Auction contract is at

::

      /path/to/plutus-pioneer-program/repo/code/week01/src/Week01/EnglishAuction.hs

We will run this contract in our local Plutus Playground.

If all went well above, you should be able to open the playground at
https://localhost:8009. You will likely receive a certificate error,
which can be bypassed.

.. figure:: img/plutus_playground.png
   :alt: Plutus Playground

Copy and paste the EnglishAuction.sh file contents into the playground,
replacing the existing demo contract.

When using the original tag for Week 01
(3746610e53654a1167aeb4c6294c6096d16b0502), you will need to remove the
"module" header from the script in order to compile it in the
playground. This is not required in future iterations of Plutus.

Another thing to note in this version of the playground is that fees are
not considered - this also changes in upcoming lectures working with
later Plutus commits.

.. figure:: img/playground_2.png
   :alt: Plutus Playground

Click the compile button. Once it has compiled, click the Simulate
button.

.. figure:: img/playground_3.png
   :alt: Plutus Playground

The default wallets are setup with 10 Lovelace and 10 T, where T is a
native token simulated by the script in the following lines:

.. code:: haskell

      myToken :: KnownCurrency
      myToken = KnownCurrency (ValidatorHash "f") "Token" (TokenName "T" :| [])

      mkKnownCurrencies ['myToken]

We are going to treat the token T as a non-fungible token (NFT), and
simulate this by changing the wallets such that Wallet 1 has 1 T and the
other wallets have 0 T.

Click the "Add Wallet" option, the adjust the balances accordingly:

.. figure:: img/playground_4.png
   :alt: Plutus Playground

You can see in the playground that the contract has three endpoints:
start, bid, and close.

The "Pay to Wallet" endpoint is always there by default in the
playground. It allows a simple transfer of Lovelace from one wallet to
another.

Click "start" on wallet 1, to create an action:

.. figure:: img/actions.png
   :alt: Plutus Playground

This is where the seller is going to set the rules for the auction.

The getSlot field specifies the deadline for the auction. Bidding after
this deadline will not be allowed by the contract.

Enter 20 into the getSlot field.

The spMinField specifies the minimum amount of ADA that must be bid. If
this minimum is not met by the deadline, no bid will succeed.

Enter 3 into the spMinBid field.

The last two fields - spCurrencySymbol and unTokenName specify the
currency of the NFT that is the subject of the auction. In Plutus a
native token is defined by a currency symbol and a name.

In this case, the symbol is 66 and the token name, as we have seen is T.

Enter these values into their respective fields.

.. figure:: img/actions2.png
   :alt: Plutus Playground

   alt text
We can also insert "wait" actions, to wait for a certain number of
slots. We will need to wait for at least one slot in order for the
transaction to start the auction to complete.

.. figure:: img/actions3.png
   :alt: Plutus Playground

   alt text
Now bidding can start.

Let's say that Wallets 2 and 3 want to bid for this token.

Wallet 2 is faster, and bids 3 Lovelace by invoking the bid endpoint
with the parameters as shown below.

.. figure:: img/actions4.png
   :alt: Plutus Playground

   alt text
We now insert another wait action, and a bid by Wallet 3.

.. figure:: img/actions5.png
   :alt: Plutus Playground

   alt text
Let's say that these two bids are the only bids.

We now add a wait action that will wait until slot 20, the deadline of
the auction.

.. figure:: img/actions6.png
   :alt: Plutus Playground

   alt text
At this point, anybody can invoke the "close" endpoint. The auction will
not settle on its own, it needs to be triggered by an endpoint.

When the "close" endpoint is triggered, the auction will be settled
according to the rules.

-  If there was at least one bid, the highest bidder will receive the
   token. This will always be the last bidder as the script will not
   allow bids that are not higher than the existing highest bid or bids
   that are lower than the minimum bid level.
-  If there were no bidders, Wallet 1 will get the token back.

Let's say that Wallet 1 invokes the "close" endpoint. We will add this
and also add another wait action, which we need at the end in order to
see the final transaction when we run the simulation.

.. figure:: img/actions7.png
   :alt: Plutus Playground

   alt text
Now, click the "Evaluate" button - either the one at the bottom or the
one at the top of the page.

After a little while, you will see the simulator view.

Towards the top of the page you will see the slots that are relevant to
the simulation, that is, the slots where an action occurred. Here we see
that these are slots 1,2,3,4 and 20.

Slot zero is not caused by our contract, it is the Genesis transaction
that sets up the initial balances of the wallets. There are three
outputs for this transaction.

.. figure:: img/evaluate1.png
   :alt: Plutus Playground

   alt text
The first transaction has one input and two outputs. The input is the
only UTxO that Wallet 1 has. Even though it is two tokens, 10 Lovelace
and 1 T, they sit in one UTxO. As mentioned earlier, UTxOs always need
to be consumed in their entirety, so the entire UTxO is sent as input.

The output is 10 ADA back to Wallet 1, and 1 T to the contract to hold
onto while the bidding takes place. Here you also see the script
address.

As we know from the introduction to the UTxO model, there can also be a
Datum, and there is a Datum, but this is not visible in this display.

.. figure:: img/evaluate2.png
   :alt: Plutus Playground

   alt text
So now the auction is set up, let's look at the next transaction, where
Wallet 2 makes a bid of 3 Lovelace.

There are two inputs - the script UTxO and the UTxO that Wallet 2 owns.

There are also two outputs - one giving change to Wallet 2, and the
other locking the bid into the contract.

The script validator here must make sure that Wallet 2 can't just take
the token, so it will only validate in a scenario where there is an
output where the token ends up in the contract again. Remember that in
the (E)UTxO model, all inputs and outputs are visible to the script.

.. figure:: img/evaluate3.png
   :alt: Plutus Playground

   alt text
Now let's look at the next transaction. This is where Wallet 3 bids 4
Lovelace (it is 5 Lovelace in Lars' videos, but I entered it as 4 and
I'd rather not take all those screenshots again).

The inputs here are Wallet 3's UTxO and the script address.

The outputs are the change of 6 Lovelace to Wallet 3, the updated script
with the new high bid of 4 Lovelace, and the return of Wallet 2's bid of
3 Lovelace to Wallet 2's address.

Again, the logic in the script must make sure that all of this is
handled correctly, i.e. that the new bid is higher than the previous bid
and that the token T continues to be locked in the contract along with
the new bid.

.. figure:: img/evaluate4.png
   :alt: Plutus Playground

   alt text
The last transaction is the "close" action. This one only has the script
UTxO as input. Its outputs are the successful bid of 4 Lovelace to the
seller (Wallet 1) and the transfer of the NFT to the successful bidder,
Wallet 3.

.. figure:: img/evaluate5.png
   :alt: Plutus Playground

   alt text
If we scroll down, we can now see the final balances.

.. figure:: img/evaluate6.png
   :alt: Plutus Playground

   alt text
Let's check what happens when something goes wrong.

So, if Wallet 2 makes a bid that is below the minimum bid, and Wallet 3
makes the same error.

In this scenario, both bids should fail and the seller (Wallet 1) should
get the token back.

Now we see that we have only three transactions. The Genesis transaction
is the same.

.. figure:: img/evaluate7.png
   :alt: Plutus Playground

   alt text
But now the biddings don't happen, because there is logic in the Plutus
code that determines that the bid is two low.

.. figure:: img/evaluate8.png
   :alt: Plutus Playground

   alt text
The last transaction is the close transaction. As this is a failed
auction, where there was no successful bid, this transaction returns the
NFT to Wallet 1.

.. figure:: img/evaluate9.png
   :alt: Plutus Playground

   alt text
And the balances reflect this.

.. figure:: img/evaluate10.png
   :alt: Plutus Playground

   alt text
If you scroll down further, you will find error messages, such as this
one showing that the bid from Wallet 2 was too low.

.. figure:: img/errorlog.png
   :alt: Plutus Playground

   alt text
So there you have it. A relatively realistic and complete auction
written as a Plutus smart contract.

When writing a Plutus contract it is important to realise that there are
two parts to a contract.

The first is the script that lives on the blockchain, that governs which
inputs can be consumed by a transaction and under what conditions.

The other part is the part that allows wallets to create valid
transactions that then will be validated by the on-chain script.

The nice thing about Plutus is that everything is written in Haskell and
the data types can be shared between the on-chain and the off-chain
parts.

For example, in this contract there is a datatype Auction:

.. code:: haskell

      data Auction = Auction
         { aSeller   :: !PubKeyHash
         , aDeadline :: !Slot
         , aMinBid   :: !Integer
         , aCurrency :: !CurrencySymbol
         , aToken    :: !TokenName
         } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

Then later there is the logic that defines the script that lives on the
chain - the validation logic of the script.

::

      mkAuctionValidator :: AuctionDatum -> AuctionAction -> ValidatorCtx -> Bool

Then, from line 231, is the off-chain (wallet) part.

These three data types define the parameters of the three endpoints:

.. code:: haskell

      data StartParams = StartParams
         { spDeadline :: !Slot
         , spMinBid   :: !Integer
         , spCurrency :: !CurrencySymbol
         , spToken    :: !TokenName
         } deriving (Generic, ToJSON, FromJSON, ToSchema)

      data BidParams = BidParams
         { bpCurrency :: !CurrencySymbol
         , bpToken    :: !TokenName
         , bpBid      :: !Integer
         } deriving (Generic, ToJSON, FromJSON, ToSchema)

      data CloseParams = CloseParams
         { cpCurrency :: !CurrencySymbol
         , cpToken    :: !TokenName
         } deriving (Generic, ToJSON, FromJSON, ToSchema)

Then there is the logic of the three endpoints, defined by the
functions:

.. code:: haskell

      start :: (HasBlockchainActions s, AsContractError e) => StartParams -> Contract w s e ()

      bid :: forall w s. HasBlockchainActions s => BidParams -> Contract w s Text ()

      close :: forall w s. HasBlockchainActions s => CloseParams -> Contract w s Text ()

An example of sharing code between the on-chain part and the off-chain
part is the minBid function:

.. code:: haskell

      {-# INLINABLE minBid #-}
      minBid :: AuctionDatum -> Integer
      minBid AuctionDatum{..} = case adHighestBid of
         Nothing      -> aMinBid adAuction
         Just Bid{..} -> bBid + 1

This is used during the validation both on the wallet side and on the
blockchain side. The wallet doesn't have to do this, it could just
submit the transaction, which would then fail, but it's neater that it
does.

Most of the rest of the script is boilerplate.

