Week 03 - Script Context
========================

.. note::
      This is a written version of `Lecture
      #3, Iteration #2 <https://www.youtube.com/watch?v=6_rfCCY9_gY>`__.

      In this lecture we learn about the script context (the third validation
      argument), handling time, and parameterized contracts.

      The code in this lecture uses Plutus commit ``81ba78edb1d634a13371397d8c8b19829345ce0d``.

Before We Start
---------------

Since the last lecture there has been an update to the playground, which is present in the Plutus commit we are using for this lecture (see note above).

There was an issue whereby the timeout, which has hardcoded into the playground was too short. This would cause simulations to fail if they took longer than the
hardcoded timeout.

There is now an option when you start the Plutus Playground Server which allows you to specify the timeout. The following example sets the timeout to 120 seconds.

.. code::

      plutus-playground-server -i 120s

Recap
-----

When we explained the (E)UTxO model in the first lecture, we mentioned that in
order to unlock a script address, the script attached to the address is
run, and that script gets three pieces of information - the *Datum*, the
*Redeemer* and the *Context*.

In the second lecture, we saw examples of that, and we saw how it
actually works in Haskell.

We saw the low-level implementation, where all three arguments are
represented by the ``Data`` type. We also saw that in practice this is not
used.

Instead, we use the typed version, where the datum and redeemer* can be
custom types (as long as they implement the ``IsData`` type class), and
where the third argument is of type ``ScriptContext``.

In the examples we have seen so far we have looked at the datum and
the redeemer, but we have always ignored the context. But the
context is, of course, very important. So, in this lecture we will
start looking at the context.

ScriptContext
-------------

The ``ScriptContext`` type is defined in package ``plutus-ledger-api``,
which is a package that, until now, we haven't needed. But now we do
need it, and it is included in this week's ``.cabal`` file. It is
defined in module ``Plutus.V1.Ledger.Contexts``.

.. code:: haskell

      data ScriptContext = ScriptContext { 
                  scriptContextTxInfo :: TxInfo, 
                  scriptContextPurpose :: ScriptPurpose 
            }

It is a record type with two fields.

The second field is of type ``ScriptPurpose``, which is defined in the same module. It defines for which purpose a script is being run.

.. code:: haskell

      data ScriptPurpose
         = Minting CurrencySymbol
         | Spending TxOutRef
         | Rewarding StakingCredential
         | Certifying DCert

For us, the most important is ``Spending``. This is what we have talked
about so far in the context of the (E)UTxO model. This is when a script
is run in order to validate a spending input for a transaction.

The ``Minting`` purpose comes into play when you want to define a native
token. Its purpose us to describe under which circumstances the native
token can be minted or burned.

There are also two new brand new purposes - ``Rewarding`` - related to
staking and ``Certifying`` - related to stake delegation.

The most interesting field, the one that contains the actual context, is ``scriptContextTxInfo``, which is of type
``TxInfo``, also defined in the same module.

.. code:: haskell

      data TxInfo = TxInfo
         { txInfoInputs      :: [TxInInfo] -- ^ Transaction inputs
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

It describes the spending transaction. In the (E)UTxO model, the context of validation is the spending transaction and its 
inputs and outputs. This context is expressed in the ``TxInfo`` type.

There are a couple of fields that are global to the whole transaction and in particular we have the list of all the inputs ``txInfoInputs``
and the list of all the outputs ``txInfoOutputs``. Each of those has a variety of fields to drill into each individual input or output.

We also see fields for fees ``txFee``, the forge value ``txInfoForge``, used when minting or burning native tokens.

Then we have a list of delegation certificates in ``txInfoDCert`` and a field ``txInfoWdrl`` to hold information about staking withdrawals.

The field ``txInfoValidRange``, which we will look at in much more detail in a moment, defines the slot range for which this transaction is valid.

``txInfoSignatories`` is the list of public keys that have signed this transaction.

Transactions that spend a script output need to include the datum of the script output.
The ``txInfoData`` field is a list associating datums with their respective hashes. If there is a transaction output to a script address
that carries some datum, you don't need to include the datum, you can just include the datum hash. However, scripts that spend an output do need to include the datum, in which case it will be included in the ``txInfoData`` list.

Finally, the ``txInfoId`` field is the ID of this transaction.

txInfoValidRange
~~~~~~~~~~~~~~~~

While there is a lot of information contained in this ``txInfo`` type, for
our first example of how to use the third argument to validation, we
will concentrate on the ``txInfoValidRange`` field.

This brings us to an interesting dilemma. We have stressed several times
that the big advantage that Cardano has over something like Ethereum is
that validation can happen in the wallet. But we have also noted that a
transaction can still fail on-chain following validation if, when the
transaction arrives on the blockchain, it has been consumed already by
someone else. In this case, the transaction fails without having to pay
fees.

What should never happen under normal circumstances is that a validation
script runs and then fails. This is because you can always run the
validation under exactly the same conditions in the wallet, so it would
fail before you ever submit it.

So that is a very nice feature, but it is not obvious how to manage time
in that context. Time is important, because we want to be able to
express that a certain transaction is only valid before or only valid
after a certain time has been reached.

We saw an example of this in lecture one - the auction example, where
bids are only allowed until the deadline has been reached, and the
``close`` endpoint can only be called after the deadline has passed.

That seems to be a contradiction, because time is obviously flowing. So,
when you try to validate a transaction that you are constructing in your
wallet, the time that you are doing that can, of course, be different
than the time that the transaction arrives at a node for validation. So,
it's not clear how to bring these two together so that validation is
deterministic, and to guarantee that if, and only if, validation
succeeds in the wallet, it will also succeed in the node.

The way Cardano solves that, is by adding the slot range field
``txInfoValidRange`` to a transaction, which essentially says "This
transaction is valid between *this* and *that* slot".

When a transaction gets submitted to the blockchain and validated by a
node, then before any scripts are run, some general checks are made, for
example that all inputs are present and that the balances add up, that
the fees are included and so on. 

One of those checks that happens before validation is to check that the slot range is valid. The 
node will look at the current time and check that it falls into the valid slot range of the transaction. If it does not, then validation fails immediately without
ever running the validator scripts.

So, if the pre-checks succeed, then this means that the current time does fall into the valid slot range.
This, in turn, means that we are completely deterministic again. The validation script can simply assume that it is being run at a valid slot.

By default, a script will use the infinite slot range, one that covers all slots starting from the genesis block and running until the end of time.

There is one slight complication with this, and that is that Ouroboros, the consensus protocol powering Cardano doesn't use POSIX time, it uses slots. But Plutus
uses real time, so we need to be able to convert back and forth between real time and slots. This is no problem so long as the slot time is fixed. Right now it is 
one second, so right now it is easy. 

However, this could change in the future. There could be a hard fork with some parameter change that would change the slot time. We can't know that in advance.
We don't know what the slot length will be in ten years, for example.

That means that slot intervals that are defined for transactions mustn't have a definite upper bound that is too far in the future. It must only be as far in the
future as it is possible to know what the slot length will be. This happens to be something like 36 hours. We know that if there is going to be a hard fork, we would
know about it at least 36 hours in advance.

POSIXTimeRange
~~~~~~~~~~~~~~

Let's look at this ``POSIXTimeRange`` type, which is defined in ``Plutus.V1.Ledger.Time``.

.. code:: haskell

      type POSIXTimeRange = Interval POSIXTime.

It is a type synonym for ``Interval POSIXTime`` and we see that ``Interval`` is defined by a ``LowerBound`` and an ``UpperBound``.

.. code:: haskell

      Interval
            ivFrom :: LowerBound a
            inTo   :: UpperBound a      

If we drill into ``LowerBound`` we see the constructor

.. code:: haskell

      data LowerBound a = LowerBound (Extended a) Closure

``Closure`` is a synonym for ``Bool`` and specifies whether a bound is included in the ``Interval`` or not.      

``Extended`` can be ``NegInf`` for negative infinity, ``PosInf`` for positive infinity, or ``Finite a``.

We also find some helper functions including the ``member`` function which checks if a given ``a`` is part of a given ``Interval``, so long as the type of ``a`` is a subtype 
of ``Ord``, which is the case for ``POSIXTime``.

.. code:: haskell

      member :: Ord a => a -> Interval a -> Bool
      member a i = i `contains` singleton a

``interval`` is a smart constructor for the ``Interval`` type which creates an ``Interval`` with an inclusive upper and lower bound.

.. code:: haskell

      interval :: a -> a -> Interval a
      interval s s' = Interval (lowerBound s) (upperBound s')
     
Then we have ``from`` which constructs an ``Interval`` which starts at ``a`` and lasts until eternity.

.. code:: haskell

      from :: a -> Interval a
      from s = Interval (lowerBound s) (UpperBound PosInf True)

And we have ``to``, which is the opposite. It constructs an ``Interval`` starting from the genesis block up to, and including ``a``.

.. code:: haskell

      to :: a -> Interval a
      to s = Interval (LowerBound NegInf True) (upperBound s)

``always`` is the default ``Interval`` which includes all times.

.. code:: haskell

      always :: Interval a
      always = Interval (LowerBound NegInf True) (UpperBound PosInf True)
      
And we have the opposite, ``never``, which contains no slots.

.. code:: haskell

      never :: Interval a
      never = Interval (LowerBound PosInf True) (UpperBound NegInf True)

There is also the ``singleton`` helper, which constructs an interval which consists of just one slot.

.. code:: haskell

      singleton :: a -> Interval a
      singleton s = interval s s      

The function ``hull`` gives the smallest interval containing both the given intervals.

.. code:: haskell

      hull :: Ord a => Interval a -> Interval a -> Interval a
      hull (Interval l1 h1) (Interval l2 h2) = Interval (min l1 l2) (max h1 h2)

The ``intersection`` function determines the largest interval that is contained in both the given intervals. This is an ``Interval`` that starts
from the largest lower bound of the two intervals and extends until the smallest upper bound.

.. code:: haskell

      intersection :: Ord a => Interval a -> Interval a -> Interval a
      intersection (Interval l1 h1) (Interval l2 h2) = Interval (max l1 l2) (min h1 h2)    
      
The ``overlaps`` function checks whether two intervals overlap, that is, whether there is a value that is a member of both intervals.

.. code:: haskell

      overlaps :: Ord a => Interval a -> Interval a -> Bool
      overlaps l r = isEmpty (l `intersection` r)

``contains`` takes two intervals and determines if the second interval is completely contained within the first one.

.. code:: haskell

      contains :: Ord a => Interval a -> Interval a -> Bool
      contains (Interval l1 h1) (Interval l2 h2) = l1 <= l2 && h2 <= h1

And we have the ``before`` and ``after`` functions to determine, if a given time is, respectively, before or after everything in a given ``Interval``.

.. code:: haskell

      before :: Ord a => a -> Interval a -> Bool
      before h (Interval f _) = lowerBound h < f

      after :: Ord a => a -> Interval a -> Bool
      after h (Interval _ t) = upperBound h > t

Let's have a play in the REPL.

.. code:: haskell

      Prelude Week03.Homework1> import Plutus.V1.Ledger.Interval
      Prelude Plutus.V1.Ledger.Interval Week03.Homework1>

Let's construct the ``Interval`` between 10 and 20, inclusive.

.. code:: haskell

      Prelude Plutus.V1.Ledger.Interval Week03.Homework1> interval (10 :: Integer) 20
      Interval {ivFrom = LowerBound (Finite 10) True, ivTo = UpperBound (Finite 20) True}

We can check whether a value is a member of an interval:

.. code:: haskell

      Prelude Plutus.V1.Ledger.Interval Week03.Homework1> member 9 $ interval (10 :: Integer) 20
      False
      
      Prelude Plutus.V1.Ledger.Interval Week03.Homework1> member 10 $ interval (10 :: Integer) 20
      True
      
      Prelude Plutus.V1.Ledger.Interval Week03.Homework1> member 12 $ interval (10 :: Integer) 20
      True
      
      Prelude Plutus.V1.Ledger.Interval Week03.Homework1> member 20 $ interval (10 :: Integer) 20
      True
      
      Prelude Plutus.V1.Ledger.Interval Week03.Homework1> member 21 $ interval (10 :: Integer) 20
      False
     
We can use the ``from`` constructor. Here the lower bound is again a finite slot, but the upper bound is positive infinity.

.. code:: haskell

      Prelude Plutus.V1.Ledger.Interval Week03.Homework1> member 21 $ from (30 :: Integer)
      False

      Prelude Plutus.V1.Ledger.Interval Week03.Homework1> member 30 $ from (30 :: Integer)
      True
      
      Prelude Plutus.V1.Ledger.Interval Week03.Homework1> member 300000 $ from (30 :: Integer)
      True

And the ``to`` constructor. Here the lower bound is negative infinity, while the upper bound is a finite slot number.

.. code:: haskell

      Prelude Plutus.V1.Ledger.Interval Week03.Homework1> member 300000 $ to (30 :: Integer)
      False

      Prelude Plutus.V1.Ledger.Interval Week03.Homework1> member 31 $ to (30 :: Integer)
      False
      
      Prelude Plutus.V1.Ledger.Interval Week03.Homework1> member 30 $ to (30 :: Integer)
      True

      Prelude Plutus.V1.Ledger.Interval Week03.Homework1> member 7 $ to (30 :: Integer)
      True

Now, let's try the ``intersection`` function on the ``Interval`` from 10 to 20 and the ``Interval`` from 18 to 30.

.. code:: haskell

      Prelude Plutus.V1.Ledger.Interval Week03.Homework1> intersection (interval (10 :: Integer) 20) $ interval 18 30
      Interval {ivFrom = LowerBound (Finite 18) True, ivTo = UpperBound (Finite 20) True}

As expected, we get the ``Interval`` that runs from 18 to 20, inclusive.

We can check whether one ``Interval`` contains another.

.. code:: haskell

      Prelude Plutus.V1.Ledger.Interval Week03.Homework1> contains (to (100 :: Integer)) $ interval 30 80
      True

      Prelude Plutus.V1.Ledger.Interval Week03.Homework1> contains (to (100 :: Integer)) $ interval 30 100
      True

      Prelude Plutus.V1.Ledger.Interval Week03.Homework1> contains (to (100 :: Integer)) $ interval 30 101
      False
            
We see that as soon as the second ``Interval`` extends to 101, it is no longer fully contained within the ``Interval`` that runs to 100.

However, if we check with ``overlaps``, then it will be true because there are elements, such as 40, that are contained in both intervals.

.. code:: haskell

      Prelude Plutus.V1.Ledger.Interval Week03.Homework1> overlaps (to (100 :: Integer)) $ interval 30 101
      True

      Prelude Plutus.V1.Ledger.Interval Week03.Homework1> overlaps (to (100 :: Integer)) $ interval 101 110
      False

Example - Vesting
-----------------

Imagine you want to give a gift of Ada to a child. You want the child to own the Ada, but you only want the child to have access to it he or she
turns eighteen.

Using Plutus, it is very easy to implement. As our first contract that will look at the context argument, we will
implement a contract that implements a vesting scheme. Money will be put into a script and then it can be retrieved by a certain person, but only once
a certain deadline has been reached.

We start by copying the ``IsData`` contract from lecture two into a new module called ``Vesting``. 

The first step is to think about the types for the datum and redeemer.

For datum, it makes sense to have two pieces of information, the beneficiary and the deadline. So, let's define this type:

.. code:: haskell

      data VestingDatum = VestingDatum
         { beneficiary :: PubKeyHash
         , deadline    :: POSIXTime
         } deriving Show

      PlutusTx.unstableMakeIsData ''VestingDatum

In order to know if someone can spend this script output, two pieces information are required, i.e. the beneficiary's signature and the time of the transaction. In 
this case, both those pieces of information are contained in the transaction itself. This means that we don't need any information in the redeemer, so we can just
use ``()`` for the redeemer.

.. code:: haskell

      mkValidator :: VestingDatum -> () -> ScriptContext -> Bool

We need to check two conditions.

1. That only the correct beneficiary can unlock a UTxO sitting at this
   address. This we can validate by checking that the beneficiary's
   signature is included in the transaction.
2. That this transaction is only executed after the deadline is reached.

We could probably just write this in one go, but we will write it in a
more top-down fashion and delegate to some helper functions.

.. code:: haskell

      mkValidator dat () ctx =
            mkValidator dat () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                                     traceIfFalse "deadline not reached" deadlineReached
      where
            info :: TxInfo
            info = scriptContextTxInfo ctx
      
To check that the transaction is signed by the beneficiary, we can get the public key of the beneficiary from the datum and pass it, along with the transaction
information to the ``txSignedBy`` function.

.. code:: haskell

            signedByBeneficiary :: Bool
            signedByBeneficiary = txSignedBy info $ beneficiary dat

How do we check that the deadline has passed?

.. figure:: img/iteration2/pic__00046.png

Let's consider a transaction with a validity that crosses the deadline, which is shown as the uppermost range in the above diagram.

Recall that before the validator script is run, other checks are made, including the time check. The node checks that the current time falls into the valid range of
the transaction and only then is the validator run. So we know that, if we are in the validator, the current time lies somewhere within the validity interval.

In the case of the range that crosses the deadline, the validator code cannot know whether the current time is before or after the deadline. In this case, the
validator must declare that the transaction is invalid.

The second example in the diagram, however, is fine. We still don't know what the current time is exactly, but we know that whatever the time is, it will be after the
deadline.

So, what we are checking for is that the whole validity interval is to the right of the deadline. One way to do this is to use the ``contains`` function to check
whether the validity interval is fully contained within the interval that starts from the deadline and extends until the end of time.

.. code:: haskell
      
            deadlineReached :: Bool
            deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info

Let's look back at the ``ScriptContext`` type.

.. code:: haskell

      data ScriptContext = ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }

We are not interest in the script purpose, as we know that it is a
spending script. The interesting one for us here is *TxInfo*, as this
provides both the signatures and the timing information.

So let's add a helper function that gets this for us from our third
argument - *ctx*.

.. code:: haskell

      mkValidator dat () ctx =
         traceIfFalse "beneficiary's signature missing" checkSig      &&
         traceIfFalse "deadline not reached"            checkDeadline
      where
         info :: TxInfo
         info = scriptContextTxInfo ctx
         ...
         checkSig :: Bool
         ...
         checkDeadline :: Bool
         ...

For the first helper function, *checkSig*, we must check that the
beneficiary has signed the transaction.

Here we use the ``elem`` function here from the Plutus Prelude, which is
a copy of the same function from the standard Prelude. You will recall
that this is because it is not possible to make functions in standard
Prelude INLINABLE, which is required for our validation scripts to
compile.

.. code:: haskell

      checkSig = beneficiary dat `elem` txInfoSignatories info

To check the deadline we need the *txInfoValidRange* field of *TxInfo*,
which gives us a value of type *SlotRange*.

We must check that this transaction is only submitted once the deadline
has been reached.

As we saw before, the way time is handled is that, during validation,
before any script is run, it is checked that this range that the
transaction gives actually includes the current slot.

We don't know exactly what the current slot is because the interval may
be large, but what we do know is that one of those slots is the current
time.

So, in order to make sure that the deadline has been reached, we must
check that all the slots in the slot range are after the deadline. And
one way to do this, is to ask if the valid slot range is included in the
interval that starts at the deadline and extends to the end of time.

.. code:: haskell

      checkDeadline = from (deadline dat) `contains` txInfoValidRange info

Remember that if the current slot was not in the *txInfoValidRange*,
then the validation script would not even be running.

That completes the validation logic. Let's take care of some
boilerplate.

.. code:: haskell

      data Vesting
      instance Scripts.ScriptType Vesting where
         type instance DatumType Vesting = VestingDatum
         type instance RedeemerType Vesting = ()

      inst :: Scripts.ScriptInstance Vesting
      inst = Scripts.validator @Vesting
         $$(PlutusTx.compile [|| mkValidator ||])
         $$(PlutusTx.compile [|| wrap ||])
      where
         wrap = Scripts.wrapValidator @VestingDatum @()

We will focus more on the wallet part of the script later, but here are
the changes.

We have created a *GiveParams* type, and modified the *grab* endpoint to
require no parameters.

.. code:: haskell

      data GiveParams = GiveParams
         { gpBeneficiary :: !PubKeyHash
         , gpDeadline    :: !Slot
         , gpAmount      :: !Integer
         } deriving (Generic, ToJSON, FromJSON, ToSchema)

      type VestingSchema =
         BlockchainActions
            .\/ Endpoint "give" GiveParams
            .\/ Endpoint "grab" ()

For the *give* endpoint, the *Datum* is constructed from the
*GiveParams*.

.. code:: haskell

      give :: (HasBlockchainActions s, AsContractError e) => GiveParams -> Contract w s e ()
      give gp = do
         let dat = VestingDatum
                     { beneficiary = gpBeneficiary gp
                     , deadline    = gpDeadline gp
                     }
            tx  = mustPayToTheScript dat $ Ada.lovelaceValueOf $ gpAmount gp
         ledgerTx <- submitTxConstraints inst tx
         void $ awaitTxConfirmed $ txId ledgerTx
         logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
            (gpAmount gp)
            (show $ gpBeneficiary gp)
            (show $ gpDeadline gp)

The *grab* endpoint is a bit more involved. Here, the grabber needs to
find the UTxOs that they can actually consume, which is performed by the
*isSuitable* helper function.

This looks at the all UTxOs and only keeps those that are suitable. It
first checks that the *Datum* hash exists, nad, if so, it deserialises
it, and, if that succeeds it checks that the beneficiary of the UTxO is
the public key hash of the grabber. It then checks that the deadline is
not in the future.

We see here that, from the wallet, we have access to the current slot
and to our own public key hash.

.. code:: haskell

      grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => Contract w s e ()
      grab = do
         now   <- currentSlot
         pkh   <- pubKeyHash <$> ownPubKey
         utxos <- Map.filter (isSuitable pkh now) <$> utxoAt scrAddress
         if Map.null utxos
            then logInfo @String $ "no gifts available"
            else do
                  let orefs   = fst <$> Map.toList utxos
                     lookups = Constraints.unspentOutputs utxos  <>
                              Constraints.otherScript validator
                     tx :: TxConstraints Void Void
                     tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | oref <- orefs] <>
                              mustValidateIn (from now)
                  ledgerTx <- submitTxConstraintsWith @Void lookups tx
                  void $ awaitTxConfirmed $ txId ledgerTx
                  logInfo @String $ "collected gifts"
      where
         isSuitable :: PubKeyHash -> Slot -> TxOutTx -> Bool
         isSuitable pkh now o = case txOutDatumHash $ txOutTxOut o of
            Nothing -> False
            Just h  -> case Map.lookup h $ txData $ txOutTxTx o of
                  Nothing        -> False
                  Just (Datum e) -> case PlutusTx.fromData e of
                     Nothing -> False
                     Just d  -> beneficiary d == pkh && deadline d <= now

Note the call:

.. code:: haskell

      mustValidateIn (from now)

If we do not do this, the default would be the infinite slot range, and
this would cause validation to fail in our case.

We could use a singleton slot here, but, if there were any issues, for
example network delays, and the transaction arrived at a node a slot or
two later, then validation would no longer work.

Another thing to note is that, if there is no suitable UTxO available,
we don't even try to submit the transaction. We want to make sure that
when the grabber submits, they get something in return. Otherwise they
would have to pay fees for a transaction that doesn't have any outputs.

In the playground
~~~~~~~~~~~~~~~~~

First, let's get Wallet 1 to send some lovelace.

.. figure:: img/week03__00004.png
   :alt: 

Here we run into a technical problem. We need to supply the beneficiary
address, but there is no way in the playground to get the public key
hash of a wallet.

But we can get it from the REPL.

.. code:: haskell

      Prelude Week03.IsData> import Wallet.Emulator
      Prelude Wallet.Emulator Week03.IsData> import Ledger
      Prelude Wallet.Emulator Ledger Week03.IsData> pubKeyHash $ walletPubKey $ Wallet 2
      39f713d0a644253f04529421b9f51b9b08979d08295959c4f3990ee617f5139f

Let's create a scenario where validation passes. Wallet 1 gives 500
lovelace with a deadline of slot 15. We wait for 15 slots, and then
Wallet 2 grabs.

.. figure:: img/week03__00005.png
   :alt: 

After evaluation, we see the Genesis transaction, plus the give and the
grab transactions.

.. figure:: img/week03__00006.png
   :alt: 

.. figure:: img/week03__00007.png
   :alt: 

.. figure:: img/week03__00008.png
   :alt: 

And the final balances.

.. figure:: img/week03__00009.png
   :alt: 

Now let's look at the case where the grab happens too early. We'll
change the wait time to 14 slots.

.. figure:: img/week03__00010.png
   :alt: 

Now we see just two transactions - the Genesis transaction, and the
give.

.. figure:: img/week03__00012.png
   :alt: 

The grab transaction has failed validation.

.. figure:: img/week03__00013.png
   :alt: 

Example 2 - Parameterized Contract
----------------------------------

Our next example will be parameterized contracts, but let's start with
an observation about our existing contract.

An Observation
~~~~~~~~~~~~~~

We will set up a scenario where both wallets give and both wallets grab.

Again, in this example, the public key hash of Wallet 1's address was
obtained from the REPL in the same way as with the Wallet 2 example
above.

.. figure:: img/week03__00014.png
   :alt: 

After evaluation...

The Genesis transaction, as always.

.. figure:: img/week03__00015.png
   :alt: 

The give of Wallet 2...

.. figure:: img/week03__00016.png
   :alt: 

The give of Wallet 1...

.. figure:: img/week03__00017.png
   :alt: 

The grab of Wallet 2...

.. figure:: img/week03__00018.png
   :alt: 

And, the grab of Wallet 1...

.. figure:: img/week03__00019.png
   :alt: 

Now, what we want to focus on here is the script addresses for the give
of Wallet 1 and the give of Wallet 2. If you look back at those
screenshots, you will notice that the script address in both cases is
the same.

And this is not surprising. Recall that the address of the script is
calculated by taken the hash of the compiled Plutus code of the
validator. Since the same validator is being used in both those
transactions, the script address is the same.

Keep this in mind for what we are about to cover in the following
section.

Another Way of Doing It
~~~~~~~~~~~~~~~~~~~~~~~

In our example, we have put the beneficiary and the deadline into the
datum. But there are other choices.

You could also parameterize the whole script on those two pieces of data
- the beneficiary and the deadline.

A parameterized script is like a family of scripts. You can instantiate
it with different parameters, and you get different scripts. They all
behave the same, but they have these different parameters.

We start by making a copy of Vesting.hs and creating a new module -
Week03.Parameterized.

Now, instead of using the *VestedDatum*, we are going to parameterize
the script with it. It makes sense to first change its name.

.. code:: haskell

      data VestingParam = VestingParam
         { beneficiary :: PubKeyHash
         , deadline    :: Slot
         } deriving Show

Next, we will return to using Unit as our datum type, but we will add a
new validation argument, before the other arguments, of our new type
*VestingParam*.

.. code:: haskell

      mkValidator :: VestingParam -> () -> () -> ScriptContext -> Bool

The idea is that mkValidator is now a function that takes a VestingParam
and returns a custom validator based on those params.

We don't need to change much, just the function header and the parts
that previously accessed the datum.

.. code:: haskell

      mkValidator :: VestingParam -> () -> () -> ScriptContext -> Bool
      mkValidator p () () ctx =
         traceIfFalse "beneficiary's signature missing" checkSig      &&
         traceIfFalse "deadline not reached"            checkDeadline
      where
         info :: TxInfo
         info = scriptContextTxInfo ctx

         checkSig :: Bool
         checkSig = beneficiary p `elem` txInfoSignatories info

         checkDeadline :: Bool
         checkDeadline = from (deadline p) `contains` txInfoValidRange info

And, we need to change another piece of code that previously referenced
the datum.

.. code:: haskell

      data Vesting
      instance Scripts.ScriptType Vesting where
         type instance DatumType Vesting = ()
         type instance RedeemerType Vesting = ()

And now we come to an interesting question. What do we do here?

.. code:: haskell

      inst :: Scripts.ScriptInstance Vesting
      inst = Scripts.validator @Vesting
         $$(PlutusTx.compile [|| mkValidator ||])
         $$(PlutusTx.compile [|| wrap ||])
      where
         wrap = Scripts.wrapValidator @VestingDatum @()

As is, this won't work because now *mkValidator* has the wrong type.
Remember that it must be a function that takes three arguments and
returns a boolean. But now, it has four arguments.

Also, we won't always get the same instance, so this must now become a
function that takes *VestingParam* as an argument.

.. code:: haskell

      inst :: VestingParam -> Scripts.ScriptInstance Vesting
      inst p = Scripts.validator @Vesting

The first idea would be to simply do something like this - adding the
*p* as a parameter, which would make the type correct again.

.. code:: haskell

      -- this won't work
      $$(PlutusTx.compile [|| mkValidator p ||])

But the problem is that, as we have seen before, in Template Haskell,
the things inside the Oxford Brackets must be known at compile time, but
the value of *p* here will not be known until runtime.

Luckily, there is a way around this.

We have something called applyCode, which takes two Plutus scripts, and,
assuming that the first one is a function, it applies this function to
the second argument.

.. code:: haskell

      -- partial code
      ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` -- ...

So, now, this...

.. code:: haskell

      ($$(PlutusTx.compile [|| mkValidator ||])

...is now a Plutus script for a function that takes such a parameter.
So, now, we must write a Plutus script for that parameter. Then
*applyCode* will apply the function to the script for the parameter, and
we will get a script of the right type out of that.

But this looks like it still doesn't solve the problem because what do
we write after *applyCode*? How do we get the parameter there. We can't
use PlutusTx.compile, as we have already seen.

This is where another important class comes in - the so-called *Lift*
class.

The Lift Class
^^^^^^^^^^^^^^

The *Lift* class is defined in package *plutus-tx*.

.. code:: haskell

      module PlutusTx.Lift.Class

It only has one function, *Lift*. However, we won't use this function
directly.

The importance of the class is that it allows us to, at runtime, lift
Haskell values into corresponding Plutus script values. And this is
exactly what we need to convert our parameter *p* into code.

We will use a different function, defined in the same package but in a
different module.

.. code:: haskell

      module PlutusTx.Lift

The function we will use is called *liftCode*

.. code:: haskell

      -- | Get a Plutus Core program corresponding to the given value as a 'CompiledCodeIn', throwing any errors that occur as exceptions and ignoring fresh names.
      liftCode
         :: (Lift.Lift uni a, Throwable uni fun, PLC.ToBuiltinMeaning uni fun)
         => a -> CompiledCodeIn uni fun a
      liftCode x = unsafely $ safeLiftCode x

It takes a Haskell value of type *a*, provided *a* is an instance of the
*Lift* class, and turns it into a piece of Plutus script code
corresponding to the same type.

So, let's use that.

.. code:: haskell

      ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)

There is still a problem, however. We need a *Lift* instance for *p*.

Luckily, similar to how we got an instance for *IsData* there is also a
Template Haskell function for *Lift*.

.. code:: haskell

      PlutusTx.makeLift ''VestingParam

But, it still won't compile. We need another GHC extension.

.. code:: haskell

      {-# LANGUAGE MultiParamTypeClasses #-}

Now we have to some more little modifications.

.. code:: haskell

      validator :: VestingParam -> Validator
      validator = Scripts.validatorScript . inst

      scrAddress :: VestingParam -> Ledger.Address
      scrAddress = scriptAddress . validator

Changes are also necessary in the wallet part.

The *GiveParams* stay the same, but the endpoints are slightly
different, because in the *grab* endpoint earlier we only had the Unit
argument, but now we need the slot.

This is because, in order to construct the address that we grab from, we
need the params - the beneficiary and the deadline. We already now the
beneficiary, as it will be the address of the wallet that is doing the
grabbing, but we need to pass in the slot value for the deadline.

In the *give* endpoint, there are also some differences.

Whenever we need an *inst* we must pass in the params.

.. code:: haskell

      ledgerTx <- submitTxConstraints (inst p) tx

And in the *grab* endpoint, we have the additional parameter.

.. code:: haskell

      grab d = do

And we can use that to construct the parameters, along with our own
public key hash.

.. code:: haskell

      let p = VestingParam
                  { beneficiary = pkh
                  , deadline    = d
                  }

And again, when we use something like *scrAddress*, we need to pass in
the parameters.

.. code:: haskell

      utxos <- utxoAt $ scrAddress p

Now, the good thing with this is that we don't need the filter helper
function *isSuitable* anymore. Previously, we got all the UTxOs sitting
at the script address and filtered them based on beneficiary and
deadline. But now, it's much easier because the script is already
parameterized by beneficiary, so we know that this script will only hold
UTxOs that are for us.

So, all we need to do is to check that *now* is not earlier than the
deadline.

.. code:: haskell

      if now < d
         then logInfo @String $ "too early"
         else do
         ...

Back to the playground
~~~~~~~~~~~~~~~~~~~~~~

If we copy paste this new contract into the playground and setup the
same scenario as before...

.. figure:: img/week03__00020.png
   :alt: 

We can see that now, one of the disadvantages to doing it this way is
that the wallets now need to know the deadline in order to construct the
script address.

If you evaluate this, you will see that it succeeds.

.. figure:: img/week03__00021.png
   :alt: 

But now, compare the script address that Wallet 1 sends to with the
script address that Wallet 2 sends to.

.. figure:: img/week03__00022.png
   :alt: 

They are now different. The UTxOs are being held at different addresses.

This is because of the parameters. The same script but with different
parameters will have a different hash.

Whether this is a good thing or a bad thing will depend on the use case.

