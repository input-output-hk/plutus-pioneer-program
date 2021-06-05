Week 03 - Script Context
========================

.. note::
      This is a written version of `Lecture
      #3 <https://youtu.be/Lk1eIVm_ZTQ>`__.

      In this lecture we learn about the script context (the third validation
      argument), handling time, and parameterized contracts.

Housekeeping
------------

For this lecture we will be working with a later commit of Plutus than
in previous lectures. You will find the commit in the cabal.project file
for Week03.

::

      cd /path/to/Plutus
      git checkout 3aa86304e9bfc425667051a8a94db73fcdc38878

It would, of course, be better for everyone if we could keep the Plutus
dependencies stable, but this is not really possible as Plutus is
evolving very quickly while heading towards the Alonzo release, where
Plutus is fully integrated into the Cardano node.

If we wait too long and stay on an outdated version, then when we
finally have to upgrade to use Plutus on the testnet, there will be lots
of changes.

This does mean that some of the code from the first two lectures will
not compile against the new version.

But, luckily, the changes are not that bad.

Let's take the last example from Week 02 and port it to the new Plutus
version to see what has changed.

Porting IsData
~~~~~~~~~~~~~~

Code Changes
^^^^^^^^^^^^

The first difference is in the *mkValidator* function.

.. code:: haskell

      mkValidator :: () -> MySillyRedeemer -> ScriptContext -> Bool
      mkValidator () (MySillyRedeemer r) _ = traceIfFalse "wrong redeemer" $ r == 42

In the previous version, the third argument was called *ValidatorCtx*.
Luckily, we have not yet looked at this argument in detail.

The second change is where we create the scrAddress.

.. code:: haskell

      scrAddress :: Ledger.Address
      scrAddress = scriptAddress validator

Previously, *scrAddress* was created using *ScriptAddress* (capital S),
passing in a validator hash. This is because the address type has now
changed in order to allow a component of the address relating to a
staking address. But there is still a smart constructor *scriptAddress*
(small s).

We don't need the validator hash anymore. It still exists and we could
compute it, but we don't need it.

Playground Changes
^^^^^^^^^^^^^^^^^^

There have also been some changes in the playground.

One is a pleasant surprise. In previous lectures, we needed to remove
the *module* header in the script after copy-pasting the code into the
playground. We no longer need to do that.

Another interesting change is that fees are now considered in the
playground. They are not yet realistic. The fees are always 10 lovelace,
but in the real system the fees will depend on the memory consumption
and the time it takes to execute the validators.

But, in any case, as there is now a fee of 10 lovelace, it no longer
makes sense to have examples with such small balances in the wallets.
So, instead of starting with 10 lovelace in each wallet, we should
choose a bigger number, for example 1000.

Let's look at the changes in the playground.

.. figure:: img/week03__00000.png
   :alt: 

The Genesis transaction is the same, with the exception that the wallets
are now given 1000 lovelace each, rather than 10.

.. figure:: img/week03__00001.png
   :alt: 

Now, we see the *give* transaction, with an addition fee output of 10
lovelace. This 10 lovelace has been deducted from the UTxO that
represents the change of 300 (1000-700) for wallet 1.

.. figure:: img/week03__00002.png
   :alt: 

Now, the *grab*. Again, 10 lovelace in fees is deducted from the 700
lovelace that Wallet 2 grabbed.

.. figure:: img/week03__00003.png
   :alt: 

Recap
-----

When we explained the (E)UTxO model in Lecture One, we mentioned that in
order to unlock a script address, the script attached to the address is
run, and that script gets three pieces of information - the *Datum*, the
*Redeemer* and the *Context*.

In the second lecture, we saw examples of that, and we saw how it
actually works in Haskell.

We saw the low-level implementation, where all three arguments are
represented by the *Data* type. We also saw that in practice this is not
used.

Instead, we use the typed version, where *Datum* and *Redeemer* can be
custom types (as long as they implement the *IsData* type class), and
where the third argument is of type *ScriptContext* (previously
*ValidatorCtx*).

In the examples we have seen so far we have looked at the *Datum* and
the *Redeemer*, but we have always ignored the *Context*. But the
*Context* is, of course, very important. So, in this lecture we will
start looking at the *Context*.

ScriptContext
-------------

The *ScriptContext* type is defined in package *plutus-ledger-api*,
which is a package that, until now, we haven't needed. But now we do
need it, and it is included in this week's cabal.project file. It is
defined in module *Plutus.V1.Ledger.Contexts*.

.. code:: haskell

      data ScriptContext = ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }

It is a record type (a Haskell type where the fields are given names,
rather than being referred to only by their position and type, although
it can still be treated in such a manner if desired).

The second field is of type *ScriptPurpose*, which is defined in the
same module. It defines for which purpose a script is being run.

.. code:: haskell

      -- | Purpose of the script that is currently running
      data ScriptPurpose
         = Minting CurrencySymbol
         | Spending TxOutRef
         | Rewarding StakingCredential
         | Certifying DCert

For us, the most important is *Spending*. This is what we have talked
about so far in the context of the (E)UTxO model. This is when a script
is run in order to validate a spending input for a transaction.

The *Minting* purpose comes into play when you want to define a native
token. Its purpose us to describe under which circumstances the native
token can be minted or burned.

There are also two new brand new purposes - *Rewarding* - related to
staking and *Certifying* - related to stake delegation.

The most interesting field is *scriptContextTxInfo* which is of type
*TxInfo*, also defined in the same module.

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

It describes the spending transaction. In the (E)UTxO model, the context
of validation is the spending transaction and its inputs and outputs.
This context is expressed in the *TxInfo* type.

In particular you will see the list of all the inputs (*txInfoInputs*)
and the list of all the outputs (*txInfoOutputs*), whose types provide a
variety of fields to drill into each individual input or output.

We also see fields for fees *txFee*, the forge value *txInfoForge*, used
when minting or burning native tokens.

The field *txInfoValidRange*, which we will come to in a moment, defines
the slot range for which this transaction is valid.

The *txInfoData* field is a list associating *Datums* with their
respective hashes. If there is a transaction output to a script address
that carries some *Datum*, you don't need to include the *Datum*, you
can just include the *Datum* hash. However, you can optionally attach
the *Datum*, in which case it will be done in the *txInfoData* list.

The *txInfoId* field is a hash of the transaction including all its
inputs and outputs.

txInfoValidRange
~~~~~~~~~~~~~~~~

While there is a lot of information contained in this *txInfo* type, for
our first example of how to use the third argument to validation, we
will concentrate on the *txInfoValidRange* field.

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
*close* endpoint can only be called after the deadline has passed.

That seems to be a contradiction, because time is obviously flowing. So,
when you try to validate a transaction that you are constructing in your
wallet, the time that you are doing that can, of course, be different
than the time that the transaction arrives at a node for validation. So,
it's not clear how to bring these two together so that validation is
deterministic, and to guarantee that if, and only if, validation
succeeds in the wallet, it will also succeed at the node.

The way Cardano solves that, is by adding the slot range field
*txInfoValidRange* to a transaction, which essentially says "This
transaction is valid between *this* and *that* slot".

When a transaction gets submitted to the blockchain and validated by a
node, then before any scripts are run, some general checks are made, for
example that all inputs are present and that the balances add up, that
the fees are included and so on. One of those checks is to check that
the slot range is valid.

This means that we are completely deterministic again because if the
script is run, we know that we are within the valid slot range.

By default, a script will use the infinite slot range, one that covers
all slots until the end of time, but we do have the option to set a
different slot range, and that is what we have to do if we deal with
time-critical smart contracts, like in the auction example.

So, let's look at this slot range type in more detail.

Slot
~~~~

One relevant module, found in package *plutus-ledger-api* is:

.. code:: haskell

      Plutus.V1.Ledger.Slot

When we look at the file in which *Slot* is defined, we see that it is a
type wrapper around *Integer*.

.. code:: haskell

      -- | The slot number. This is a good proxy for time, since on the Cardano blockchain
      -- slots pass at a constant rate.
      newtype Slot = Slot { getSlot :: Integer }
         deriving stock (Haskell.Eq, Haskell.Ord, Show, Generic)
         deriving anyclass (FromJSON, FromJSONKey, ToJSON, ToJSONKey, NFData)
         deriving newtype (Haskell.Num, AdditiveSemigroup, AdditiveMonoid, AdditiveGroup, Enum, Eq, Ord, Real, Integral, Serialise, Hashable, PlutusTx.IsData)

In order to construct a value of type *Slot*, we can use the *Slot*
constructor, but it's even easier if you look at the implemented type
classes, where we can see that it also implements the *Num* type class,
which means that we can use numeric literals, so we can simply write 17,
for example, rather than "Slot 17", or "Slot {getSlot=17}".

The definition of *SlotRange* is

.. code:: haskell

      -- | An 'Interval' of 'Slot's.
      type SlotRange = Interval Slot

So *SlotRange* is an *Interval Slot* - so what is *Interval*? That is
defined in a module in the same package - *plutus-ledger-api*.

This is more general and is not necessarily for *Slot*\ s. Here, we are
only concerned with the case where the type variable *a* is *Slot*.

.. code:: haskell

      --   The interval can also be unbounded on either side.
      data Interval a = Interval { ivFrom :: LowerBound a, ivTo :: UpperBound a }
         deriving stock (Haskell.Eq, Haskell.Ord, Show, Generic)
         deriving anyclass (FromJSON, ToJSON, Serialise, Hashable, NFData)

There are some slight complications. For example, you can specify
whether one or both of the bounds are inclusive, and you have the
special case where the upper bound is infinity and the case where the
lower bound is the beginning of time.

Normally, we don't have to deal with types directly because we have nice
helper functions. The most general of these helper functions is probably
the *interval* function, which takes an inclusive lower bound and an
inclusive upper bound and constructs an interval from those values.

The comment on this function in the commit we are working with in this
lecture is incorrect - it claims that the upper bound is not inclusive,
but it actually is.

.. code:: haskell

      interval :: a -> a -> Interval a
      interval s s' = Interval (lowerBound s) (upperBound s')

There is also the *singleton* helper, which constructs an interval which
consists of just one slot.

.. code:: haskell

      singleton :: a -> Interval a
      singleton s = interval s s

We have *from* which constructs an *Interval* starting from a given slot
and extending to the end of time.

.. code:: haskell

      from :: a -> Interval a
      from s = Interval (lowerBound s) (UpperBound PosInf True)

And we have *to*, which is the opposite. It constructs an *Interval*
starting from the genesis block up to, and including, the given slot.
Again, the comments in the code for the commit we are working with
claims that it is not inclusive, but it is.

.. code:: haskell

      to :: a -> Interval a
      to s = Interval (LowerBound NegInf True) (upperBound s)

We have *always* which contains all slots from the beginning of time
until the end of eternity. This is the default.

.. code:: haskell

      always :: Interval a
      always = Interval (LowerBound NegInf True) (UpperBound PosInf True)

And we have the opposite, *never*, which contains no slots.

.. code:: haskell

      never :: Interval a
      never = Interval (LowerBound PosInf True) (UpperBound NegInf True)

In addition to these helper functions for constructing values of type
*Interval*, we have various helpers for working with *Interval*\ s.

The *member* function checks whether a value is contained within an
*Interval*.

.. code:: haskell

      member :: Ord a => a -> Interval a -> Bool
      member a i = i `contains` singleton a

The *overlaps* function checks whether two intervals overlap, that is,
whether there is a value that is a member of both intervals.

.. code:: haskell

      overlaps :: Ord a => Interval a -> Interval a -> Bool
      overlaps l r = isEmpty (l `intersection` r)

The *intersection* function determines the largest interval that is
contained in both the given intervals. This is an *Interval* that starts
from the largest lower bound of the two intervals and extends until the
smallest upper bound.

.. code:: haskell

      intersection :: Ord a => Interval a -> Interval a -> Interval a
      intersection (Interval l1 h1) (Interval l2 h2) = Interval (max l1 l2) (min h1 h2)

The function *hull* gives the smallest interval containing both the
given intervals.

.. code:: haskell

      hull :: Ord a => Interval a -> Interval a -> Interval a
      hull (Interval l1 h1) (Interval l2 h2) = Interval (min l1 l2) (max h1 h2)

The *contains* function takes two intervals and determines if the second
interval is completely contained within the first one.

.. code:: haskell

      contains :: Ord a => Interval a -> Interval a -> Bool
      contains (Interval l1 h1) (Interval l2 h2) = l1 <= l2 && h2 <= h1

And we have the *before* and *after* functions to determine if a given
*Slot* is before or after a given *Interval*, respectively.

.. code:: haskell

      before :: Ord a => a -> Interval a -> Bool
      before h (Interval f _) = lowerBound h < f

      after :: Ord a => a -> Interval a -> Bool
      after h (Interval _ t) = upperBound h > t

Let's have a play in the REPL.

.. code:: haskell

      Prelude Week03.IsData> import Plutus.V1.Ledger.Slot 
      Prelude Plutus.V1.Ledger.Slot Week03.IsData> import Plutus.V1.Ledger.Interval 

There are two ways to define a slot. First, you can use the *Slot*
constructor.

.. code:: haskell

      Prelude Plutus.V1.Ledger.Slot Plutus.V1.Ledger.Interval Week03.IsData> Slot 3
      Slot {getSlot = 3}

Secondly, you can just write it as an *Integer*, but in this case you
need to tell the compiler what type it is.

.. code:: haskell

      Prelude Plutus.V1.Ledger.Slot Plutus.V1.Ledger.Interval Week03.IsData> 3 :: Slot
      Slot {getSlot = 3}

Let's use some of the helper functions for constructing intervals. This
will give us slots 3,4,5,6,7,8,9,10:

.. code:: haskell

      Prelude Plutus.V1.Ledger.Slot Plutus.V1.Ledger.Interval Week03.IsData> interval (Slot 3) 10
      Interval {ivFrom = LowerBound (Finite (Slot {getSlot = 3})) True, ivTo = UpperBound (Finite (Slot {getSlot = 10})) True}

You see that there are two finite slots defined as the lower and upper
bounds, and that they both have the value *True*, which indicates that
they are both inclusive bounds.

We can check whether a slot is a member of an interval:

.. code:: haskell

      Prelude Plutus.V1.Ledger.Slot Plutus.V1.Ledger.Interval Week03.IsData> member 5 $ interval (Slot 3) 10
      True

      Prelude Plutus.V1.Ledger.Slot Plutus.V1.Ledger.Interval Week03.IsData> member 3 $ interval (Slot 3) 10
      True

      Prelude Plutus.V1.Ledger.Slot Plutus.V1.Ledger.Interval Week03.IsData> member 10 $ interval (Slot 3) 10
      True

      Prelude Plutus.V1.Ledger.Slot Plutus.V1.Ledger.Interval Week03.IsData> member 11 $ interval (Slot 3) 10
      False

We can use the *from* constructor. Here we see that the lower bound is
again a finite slot, but that the upper bound is positive infinity.

.. code:: haskell

      Prelude Plutus.V1.Ledger.Slot Plutus.V1.Ledger.Interval Week03.IsData> from (Slot 20)
      Interval {ivFrom = LowerBound (Finite (Slot {getSlot = 20})) True, ivTo = UpperBound PosInf True}

And we can check slots for membership of this interval:

.. code:: haskell

      Prelude Plutus.V1.Ledger.Slot Plutus.V1.Ledger.Interval Week03.IsData> member 20 $ from (Slot 20)
      True

      Prelude Plutus.V1.Ledger.Slot Plutus.V1.Ledger.Interval Week03.IsData> member 19 $ from (Slot 20)
      False

      Prelude Plutus.V1.Ledger.Slot Plutus.V1.Ledger.Interval Week03.IsData> member 1000000 $ from (Slot 20)
      True

And the *to* constructor. Here we see that now the lower bound is
negative infinity, while the upper bound is a finite slot number.

.. code:: haskell

      Prelude Plutus.V1.Ledger.Slot Plutus.V1.Ledger.Interval Week03.IsData> to (Slot 100)
      Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound (Finite (Slot {getSlot = 100})) True}

And let's check various slots for membership:

.. code:: haskell

      Prelude Plutus.V1.Ledger.Slot Plutus.V1.Ledger.Interval Week03.IsData> member 7 $ to (Slot 100)
      True

      Prelude Plutus.V1.Ledger.Slot Plutus.V1.Ledger.Interval Week03.IsData> member 100 $ to (Slot 100)
      True

      Prelude Plutus.V1.Ledger.Slot Plutus.V1.Ledger.Interval Week03.IsData> member 101 $ to (Slot 100)
      False

Now, let's try the *contains* function:

.. code:: haskell

      Prelude Plutus.V1.Ledger.Slot Plutus.V1.Ledger.Interval Week03.IsData> contains (to $ Slot 100) $ interval 30 50
      True

      Prelude Plutus.V1.Ledger.Slot Plutus.V1.Ledger.Interval Week03.IsData> contains (to $ Slot 100) $ interval 30 110
      False

And *overlaps*:

.. code:: haskell

      Prelude Plutus.V1.Ledger.Slot Plutus.V1.Ledger.Interval Week03.IsData> overlaps (to $ Slot 100) $ interval 30 110
      True

      Prelude Plutus.V1.Ledger.Slot Plutus.V1.Ledger.Interval Week03.IsData> overlaps (to $ Slot 100) $ interval 101 110
      False

And now, we can look, for the first time, at a contract that actually
looks at the third validation argument, the *Context*, and does
something interesting with it.

Example - Vesting
-----------------

Imagine you want to give a gift of Ada to a child. You want the child to
own the Ada, but you only want the child to have access to it he or she
turns eighteen.

Using Plutus, it is very easy to implement a vesting scheme like that.

We start by copying the IsData function, the one we modified at the
start of the lecture, into a new module called Vesting.

The first step is to think about the *Datum* and *Redeemer*.

For *Datum* it makes sense to have two pieces of information:

-  The beneficiary
-  The deadline

So, let's define this type:

.. code:: haskell

      data VestingDatum = VestingDatum
         { beneficiary :: PubKeyHash
         , deadline    :: Slot
         } deriving Show

      PlutusTx.unstableMakeIsData ''VestingDatum

In this case, we don't need any information in the *Redeemer*, because
all the information we need about the entity that can claim the Ada and
the time is contained in the *Context*.

.. code:: haskell

      mkValidator :: VestingDatum -> () -> ScriptContext -> Bool

We need to check two conditions.

1. That only the correct beneficiary can unlock a UTxO sitting at this
   address. This we can validate by checking that the beneficiary's
   signature is included in the transaction.
2. That this transaction is only executed after the deadline is reached.

We could probably just write this in one go, but we will write it in a
more top-down fashion and delegate to some helper functions.

Let's start by writing the conditions without implementing them and by
also giving appropriate error messages.

.. code:: haskell

      mkValidator dat () ctx =
         traceIfFalse "beneficiary's signature missing" checkSig      &&
         traceIfFalse "deadline not reached"            checkDeadline
      where
         ...
         checkSig :: Bool
         ...
         checkDeadline :: Bool
         ...

Let's look back at the *ScriptContext* type.

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

