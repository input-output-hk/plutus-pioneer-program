.. highlight:: haskell
.. _contract_models_tutorial:

Testing Plutus Contracts with Contract Models
=============================================

Introduction
------------

In this tutorial we will see how to test Plutus contracts with
*contract models*, using the framework provided by
:hsmod:`Plutus.Contract.Test.ContractModel` This framework generates
and runs tests on the Plutus emulator, where each test may involve a
number of emulated wallets, each running a collection of Plutus
contracts, all submitting transactions to an emulated blockchain.
Once the user has defined a suitable model, then QuickCheck can
generate and run many thousands of scenarios, taking the application
through a wide variety of states, and checking that it behaves
correctly in each one. Once the underlying contract model is in place,
then the framework can check user-defined properties specific to the
application, generic properties such as that no funds remain locked in
contracts for ever, and indeed both positive and negative
tests---where positive tests check that the contracts allow the
intended usages, and negative tests check that they do *not* allow
the unintended ones.

The `ContractModel` framework is quite rich in features, but we will
introduce them gradually and explain how they can best be used.

Basic Contract Models
---------------------

Example: A Simple Escrow Contract
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We begin by showing how to construct a model for a simplified escrow
contract, which can be found in
:hsmod:`Plutus.Contracts.Tutorial.Escrow`. This contract enables a
group of wallets to make a predetermined exchange of tokens, for
example selling an NFT for Ada. There are two endpoints, a
:hsobj:`Plutus.Contracts.Tutorial.Escrow.pay` endpoint, and a
:hsobj:`Plutus.Contracts.Tutorial.Escrow.redeem` endpoint.
Each wallet pays in its contribution to the contract using the
:hsobj:`Plutus.Contracts.Tutorial.Escrow.pay` endpoint, and once all
the wallets have done so, then any wallet can trigger the
predetermined payout using the
:hsobj:`Plutus.Contracts.Tutorial.Escrow.redeem` endpoint.

For simplicity, we will begin by testing the contract for a fixed set
of predetermined payouts.
These are defined by the
:hsobj:`Plutus.Contracts.Tutorial.Escrow.EscrowParams`, a
type exported by the escrow contract, and which is actually passed to
the on-chain validators. :hsmod:`Plutus.Contract.Test` provides ten
emulated wallets for use in tests, :hsobj:`Plutus.Contract.Test.w1`
to :hsobj:`Plutus.Contract.Test.w10`; in this case we
will use five of them:

.. literalinclude:: Escrow.hs
   :start-after: START testWallets
   :end-before: END testWallets

Let us decide arbitrarily that :hsobj:`Plutus.Contract.Test.w1` will
receive a payout of 10 Ada, and :hsobj:`Plutus.Contract.Test.w2` will
receive a payout of 20, and define an
:hsobj:`Plutus.Contracts.Tutorial.Escrow.EscrowParams`
value to represent that:

.. literalinclude:: Escrow.hs
   :start-after: START escrowParams
   :end-before: END escrowParams

The Contract Model Type
^^^^^^^^^^^^^^^^^^^^^^^

In order to generate sensible tests, and to decide how they should
behave, we need to track the expected state of the system. The first
step in defining a contract model is to define a type to represent
this expected state. We usually need to refine it as the model
evolves, but for now we keep things simple.

In this case, as wallets make payments into the escrow, we will need
to keep track of how much each wallet has paid in. So let us define an
``EscrowModel`` type that records these contributions. Once the
contributions reach the targets, then the escrow may be redeemed, so
let us keep track of these targets in the model too. We define

.. literalinclude:: Escrow.hs
   :start-after: START EscrowModel
   :end-before: END EscrowModel

Note that we use `lenses <http://hackage.haskell.org/package/lens>`_
to access the fields of the model. This is why the field names begin
with an underscore; the ``makeLenses`` call creates lenses called just
``contributions`` and ``targets`` for these fields, which we will use
to access and modify the fields below.

We turn this type into a contract model by making it an instance of
the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.ContractModel` class:

.. literalinclude:: Escrow.hs
   :start-after: START ContractModelInstance
   :end-before: END ContractModelInstance

The rest of the contract model is provided by defining the methods and
associated data types of this class.

What contracts shall we test?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In general, a contract model can be used to test any number of
contracts, of differing types, running in any of the emulated
wallets. But we need to tell the framework *which* contracts we are
going to test, and we need a way for the model to refer to each
*contract instance*, so that we can invoke the right endpoints. We do
so using a :hsobj:`Plutus.Contract.Test.ContractModel.Interface.ContractInstanceKey`, but since different models will be
testing different collections of contracts, then this type is not
*fixed*, it is defined as part of each model, as an associated type of
the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.ContractModel` class.

In this case only one kind of contract is involved in the tests, the
escrow contract, but there will be many instances of it, one running
in each wallet. To identify a contract instance, a
:hsobj:`Plutus.Contract.Test.ContractModel.Interface.ContractInstanceKey` just has to record the wallet it is running
in, we only need one constructor in the type. In general there will be
one constructor for each type of contract instance in the test.

.. literalinclude:: Escrow.hs
   :start-after: START ContractInstanceKeyType
   :end-before: END ContractInstanceKeyType

Note that the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.ContractInstanceKey` type is a GADT, so it tracks not only the
model type it belongs to, but also the type of the contract instance
it refers to.

The framework also needs to be able to show and compare
:hsobj:`Plutus.Contract.Test.ContractModel.Interface.ContractInstanceKey`, so you might expect that we would add a
deriving clause to this type definition. But a deriving clause is
actually not supported here, because the type is a GADT, so instead we
have to give separate 'standalone deriving' declarations outside the
:hsobj:`Plutus.Contract.Test.ContractModel.Interface.ContractModel` instance:

.. literalinclude:: Escrow.hs
   :start-after: START ContractInstanceKeyDeriving
   :end-before: END ContractInstanceKeyDeriving


Defining :hsobj:`Plutus.Contract.Test.ContractModel.Interface.ContractInstanceKey` is only part of the story: we also
have to tell the framework how to *interpret* the contract instance
keys, in particular


#.  which contract instances to start
#.  which emulated wallets to run them in
#.  which actual contract each contract instance should run.


We do so by defining three methods in the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.ContractModel` class:


.. literalinclude:: Escrow.hs
   :start-after: START ContractKeySemantics
   :end-before: END ContractKeySemantics

The first line above tells the test framework to start a contract
instance in each of the test wallets (with contract parameter ``()``),
the second line tells the framework which wallet each contract key
should run in, and the third line tells the framework which contract
to run for each key--in this case, the same ``testContract`` in each
wallet. ``Spec.Tutorial.Escrow`` does not actually export a complete
concrete, only contract endpoints, so for the purposes of the test we
just define a contract that allows us to invoke those endpoints
repeatedly:

.. literalinclude:: Escrow.hs
   :start-after: START testContract
   :end-before: END testContract


What actions should tests perform?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The type of Actions
'''''''''''''''''''

The final *type* we need to define as part of a contract model tells
the framework what *actions* to include in generated tests. This is
defined as another associated datatype of the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.ContractModel` class,
and in this case, we will just need actions to invoke the two contract
endpoints:

.. literalinclude:: Escrow.hs
   :start-after: START ActionType
   :end-before: END ActionType

The framework needs to be able to show and compare
:hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action` too, but in this case we *can* just add a ``deriving``
clause to the definition.

Performing Actions
''''''''''''''''''

QuickCheck will generate sequences of :hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action` as tests, but in
order to *run* the tests, we need to specify how each action should be
performed. This is done by defining the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.perform` method of the
:hsobj:`Plutus.Contract.Test.ContractModel.Interface.ContractModel` class, which maps :hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action` to a computation in the
emulator. :hsobj:`Plutus.Contract.Test.ContractModel.Interface.perform` takes several parameters besides the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action` to
perform, but for now we ignore all but the first, whose purpose is to
translate a :hsobj:`Plutus.Contract.Test.ContractModel.Interface.ContractInstanceKey`, used in the model, into a
:hsobj:`Plutus.Trace.Emulator.Types.ContractHandle`, used to refer to a contract instance in the
emulator. The :hsobj:`Plutus.Contract.Test.ContractModel.Interface.perform` method is free to use any :hsobj:`Plutus.Trace.Emulator.EmulatorTrace`
operations, but in practice we usually keep it simple, interpreting
each :hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action` as a single call to a contract endpoint. This gives
QuickCheck maximal control over the interaction between the tests and
the contracts. In this case, we just call either the :hsobj:`Plutus.Contracts.Tutorial.Escrow.pay` or the
:hsobj:`Plutus.Contracts.Tutorial.Escrow.redeem` endpoint.

.. literalinclude:: Escrow.hs
   :start-after: START perform
   :end-before: END perform

Notice that we *do* need to allow each :hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action` time to complete, so
we include a :hsobj:`Plutus.Contract.Test.ContractModel.Interface.delay` to tell the emulator to move on to the next
slot after each endpoint call. Of course we are free *not* to do this,
but then tests will submit many endpoint calls per slot, and fail
because the endpoints are not ready to perform them. This is not the
most interesting kind of test failure, and so we avoid it by delaying
an appropriate number of slots after each endpoint call. The number of
slots we need to wait varies from contract to contract, so we usually
determine these numbers experimentally. Exactly the same problem
arises in writing unit tests, of course.

Modelling Actions
'''''''''''''''''

Remember that we need to track the real state of the system using the
contract model state? We defined a type for this purpose:

.. literalinclude:: Escrow.hs
   :start-after: START EscrowModel
   :end-before: END EscrowModel

We need to tell the framework what the *effect* of each :hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action` is
expected to be, both on wallet contents, and in terms of the model state. We do
this by defining the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.nextState` method of the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.ContractModel`
class, which just takes an :hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action` as a parameter, and interprets
it in the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.Spec` monad, provided by the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.ContractModel` framework.

.. literalinclude:: Escrow.hs
   :start-after: START 0nextState
   :end-before: END 0nextState

You can see that the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.Spec` monad allows us to withdraw and deposit
values in wallets, so that the framework can predict their expected
contents, and also to read and update the model state using the lenses
generated from its type definition. For a ``Pay`` action, we withdraw
the payment from the wallet, and record the contribution in the model
state, using |%=|_ to update the ``contributions`` field. For a
``Redeem`` action, we read the targets from the model state (using
:hsobj:`Plutus.Contract.Test.ContractModel.Interface.viewContractState` and the lens generated from the type
definition), and then make the corresponding payments to the wallets
concerned. In both cases we tell the model to :hsobj:`Plutus.Contract.Test.ContractModel.Interface.wait` one slot,
corresponding to the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.delay` call in :hsobj:`Plutus.Contract.Test.ContractModel.Interface.perform`; this is necessary
to avoid the model and the emulator getting out of sync.

.. |%=| replace:: ``(%=)``
.. _%=: https://hackage.haskell.org/package/lens-5.1.1/docs/Control-Lens-Setter.html#v:-37--61-

We also have to specify the *initial* model state at the beginning of
each test: we just record that no contributions have been made yet,
along with the targets we chose for testing with.

.. literalinclude:: Escrow.hs
   :start-after: START initialState
   :end-before: END initialState

Given these definitions, the framework can predict the expected model
state after any sequence of :hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action`.

Generating Actions
^^^^^^^^^^^^^^^^^^

The last step, before we can actually run tests, is to tell the
framework how to *generate* random actions. We do this by defining the
:hsobj:`Plutus.Contract.Test.ContractModel.Interface.arbitraryAction` method, which is just a QuickCheck generator for
the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action` type. It gets the current model state as a parameter,
so we can if need be adapt the generator depending on the state, but
for now that is not important: we just choose between making a payment
from a random wallet, and invoking :hsobj:`Plutus.Contracts.Tutorial.Escrow.redeem` from a random
wallet. Since we expect to need several payments to fund a redemption,
we generate ``Pay`` actions a bit more often than ``Redeem`` ones.

.. literalinclude:: Escrow.hs
   :start-after: START arbitraryAction1
   :end-before: END arbitraryAction1

Strictly speaking the framework now has enough information to generate
and run tests, but it is good practice to define *shrinking* every
time we define *generation*; we just defined a generator for actions,
so we should define a shrinker too. We do so by defining the
:hsobj:`Plutus.Contract.Test.ContractModel.Interface.shrinkAction` method, which, like the QuickCheck |shrink|_
function, just returns a list of smaller :hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action` to try replacing
an action by when a test fails. It is always worth defining a
shrinker: the small amount of effort required is repaid *very*
quickly, since failed tests become much easier to understand.

.. |shrink| replace:: ``shrink``
.. _shrink: https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html#v:shrink

In this case, as in most others, we can just reuse the existing
shrinking for those parts of an :hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action` that make sense to
shrink. There is no sensible way to shrink a wallet, really, so we
just shrink the amount in a payment.

.. literalinclude:: Escrow.hs
   :start-after: START shrinkAction
   :end-before: END shrinkAction

With this definition, failing test cases will be reported with the
*minimum* payment value that causes a failure.


Running tests and debugging the model
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We are finally ready to run some tests! We do still need to define a
*property* that we can call QuickCheck with, but the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.ContractModel`
framework provides a standard one that we can just reuse. So we define

.. literalinclude:: Escrow.hs
   :start-after: START prop_Escrow
   :end-before: END prop_Escrow

The important information here is in the type signature, which tells
QuickCheck *which* contract model we want to generate and run tests
for.

A failing test
''''''''''''''

Once the property is defined, we are ready to test--and a test fails
immediately! This is not unexpected--it is quite rare that a model and
implementation match on the first try, so we should expect a little
debugging--*of the model*--before we start to find interesting bugs in
contracts. When models are written *after* the implementation, as in
this case, then the new code--the model code--is likely to be where
bugs appear first.

Looking at the test output, the first thing QuickCheck reports is the
failed test case:

.. code-block:: text

   Prelude Spec.Tutorial.Escrow Test.QuickCheck Main> quickCheck prop_Escrow
   *** Failed! Assertion failed (after 7 tests and 2 shrinks):
   Actions
    [Redeem (Wallet 5)]

Here we see what generated tests looks like: they are essentially
lists of :hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action`, performed in sequence. In this case there is only
one :hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action`: wallet 5 just attempted to redeem the funds in the
contract.

The next lines of output tell us why the test failed:

.. code-block:: text

   Expected funds of W[2] to change by
     Value (Map [(,Map [("",20000000)])])
   but they did not change
   Expected funds of W[1] to change by
     Value (Map [(,Map [("",10000000)])])
   but they did not change

Remember we defined the expected payout to be 10 Ada to :hsobj:`Plutus.Contract.Test.w1`, and 20
Ada to :hsobj:`Plutus.Contract.Test.w2`. Our model says (in :hsobj:`Plutus.Contract.Test.ContractModel.Interface.nextState`) that when we perform
a ``Redeem`` then the payout should be made (in Lovelace, not Ada,
which is why the numbers are a million times larger than those in the
model). But the wallets did not get the money--which is hardly
surprising since no payments at all have been made *to* the contract,
so there is no money to disburse.

The remaining output displays a log from the failing contract
instance, and the emulator log, both containing the line

.. code-block:: text

    Contract instance stopped with error: RedeemFailed NotEnoughFundsAtAddress

This is an error thrown by the off-chain :hsobj:`Plutus.Contracts.Tutorial.Escrow.redeem` endpoint code,
which (quite correctly) checks the funds available, and fails since there
are not enough.

Positive testing with preconditions
'''''''''''''''''''''''''''''''''''

We now have a failing test, that highlights a discrepancy between the
model and the implementation--and it is the model that is wrong. The
question is how to fix it, and there is a choice to be made. Either we
could decide that the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.nextState` function in the model should
*check* whether sufficient funds are available, and if they are not,
predict that no payments are made. Or perhaps, we should *restrict our
tests so they do not attempt to use ``Redeem`` when it should not
succeed*.

Both choices are reasonable. The first alternative is usually called
*negative testing*--we deliberately test error situations, and make
sure that the implementation correctly detects and handles those
errors. The second alternative is *positive testing* (or "happy path"
testing), when we make sure that the implementation provides the
functionality that it should, when the user makes *correct* use of its
API.

It is usually a good idea to focus on positive testing first--indeed,
good positive testing is a prerequisite for good negative testing,
because it enables us to get the system into a wide variety of
interesting states (in which to perform negative tests). So we shall
return to negative testing later, and focus--in this section--on
making positive testing work well.

To do so, we have to *restrict* test cases, so that they do not
include ``Redeem`` actions, when there are insufficient funds in the
escrow. We restrict actions by defining the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.precondition` method of
the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.ContractModel` class: any :hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action` for which :hsobj:`Plutus.Contract.Test.ContractModel.Interface.precondition`
returns ``False`` will *not* be included in any generated test. The
:hsobj:`Plutus.Contract.Test.ContractModel.Interface.precondition` method is also given the current :hsobj:`Plutus.Contract.Test.ContractModel.Interface.ModelState` as a
parameter, so that it can decide to accept or reject an :hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action`
based on where it appears in a test.

In this case, we want to *allow* a ``Redeem`` action only if there are
sufficient funds in the escrow, so we just need to compare the
contributions made so far to the targets:

.. literalinclude:: Escrow.hs
   :start-after: START precondition1
   :end-before: END precondition1

In this code, ``s`` is the entire model state maintained by the
framework (including wallet contents, slot number etc), but it
contains the "contract state", which is the state we have defined
ourselves, the ``EscrowModel``. The *lens* ``contractState
. contributions . to fold`` extracts the ``EscrowModel``, extracts the
``contributions`` field from it, and then combines all the :hsobj:`Ledger.Value`
using |fold|_. When we apply it to ``s`` using |^.|_, we get
the total value of all contributions. Likewise, the second lens
application computes the combined value of all the targets. If the
contributions exceed the targets, then the ``Redeem`` is allowed;
otherwise, it will not be included in the test. Once
we define :hsobj:`Plutus.Contract.Test.ContractModel.Interface.precondition`, then it has to be defined for every form
of :hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action`, so we just add a default branch that returns ``True``.

.. |^.| replace:: ``(^.)``
.. _^.: https://hackage.haskell.org/package/lens-5.1.1/docs/Control-Lens-Getter.html#v:-94-.

.. |fold| replace:: ``fold``
.. _fold: https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Foldable.html#v:fold

.. note::

   We can't use ``(>=)`` to compare :hsobj:`Ledger.Value`; there is no
   ``Ord`` instance. That is because some :hsobj:`Ledger.Value` are incomparable,
   such as one Ada and one NFT, which would break our expectations about
   ``Ord``. That is why we have to compare them using :hsobj:`Plutus.V1.Ledger.Value.geq` instead.

With this precondition, the failing test we have seen can no longer be
generated, and will not appear again in our |quickCheck|_ runs.

.. |quickCheck| replace:: ``quickCheck``
.. _quickCheck: https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html#v:quickCheck

A second infelicity in the model
''''''''''''''''''''''''''''''''

Adding a precondition for ``Redeem`` prevents the previous failing
test from being generated, but it does not make the tests pass: it
just allows QuickCheck to reveal the next problem in the
model. Running tests again, we see:

.. code-block:: text

   Prelude Spec.Tutorial.Escrow Test.QuickCheck Main> quickCheck prop_Escrow
   *** Failed! Assertion failed (after 4 tests and 5 shrinks):
   Actions
    [Pay (Wallet 2) 0]

This time the test just consists of a single ``Pay`` action, making a
payment of zero (!) Ada to the the contract.

.. note::

   It may seem surprising that the test tries to make a zero payment,
   given that the *generator* we saw above only generates payments in
   the range 1 to 30 Ada. But remember that the failing test cases we
   see are not necessarily freshly generated, they may also have been
   *shrunk*. In this case, the zero is a result of shrinking: the
   shrinker we saw can certainly shrink payments to zero, and the
   *precondition* for ``Pay`` allows that... it's always ``True``. And
   so, a zero payment can appear in tests. If we wanted to prevent
   this, the correct way would be to tighten the precondition of
   ``Pay``.

The next part of the output explains why the test failed:

.. code-block:: text

   Expected funds of W[2] to change by
     Value (Map [])
   but they changed by
     Value (Map [(,Map [("",-2000000)])])
   a discrepancy of
     Value (Map [(,Map [("",-2000000)])])

In other words, the model expected that a payment of zero would not
affect the funds held by the calling wallet, but in fact, the wallet
lost 2 Ada.

Why did this happen? In this case, the emulator log that follows
provides an explanation:

.. code-block:: text

   .
   .
   .
   [INFO] Slot 1: W[2]: Balancing an unbalanced transaction:
          Tx:
            Tx 2dc052b47a1faeacc0f50b99359990302885a34104df0109576597cc490b8a98:
              {inputs:
              collateral inputs:
              outputs:
                - Value (Map [(,Map [("",2000000)])]) addressed to
                  ScriptCredential: bcf453ff769866e23d14d5104c36ce4da0ff5bcbed23c622f46b94f1 (no staking credential)
              mint: Value (Map [])
              fee: Value (Map [])
              mps:
              signatures:
              validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
              data:
                "\128\164\244[V\184\141\DC19\218#\188L<u\236m2\148<\b\DEL%\v\134\EM<\167"}
          Requires signatures:
          Utxo index:
          Validity range:
            (-? , +?)
   .
   .
   .

We see the transaction submitted by the contract, and we can see from
its outputs that it *is* paying 2 Ada to the script, even though we
specified a payment of zero. The reason for this is that the Cardano
blockchain requires a *minimum Ada amount* in every transaction
output, currently 2 Ada. It is therefore *impossible* to make a
payment of zero Ada to the script--and the Plutus libraries avoid this
by adding Ada to each output, if necessary, to meet the minimum
requirement. It is these 2 Ada that the wallet has lost.

This is not really a bug in the escrow contract: it's a fundamental
limitation enforced by the blockchain itself. Therefore we must adapt
our model to allow for it. Once again we have a choice: we *could*
specify that every ``Pay`` action costs at least the minimum Ada, even
if the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action` contains a lower payment, *or* we can restrict
``Pay`` actions to amounts greater than or equal to the minimum. We
choose the latter, because it is simpler to express--we just tighten
the precondition for ``Pay``:

.. literalinclude:: Escrow.hs
   :start-after: START precondition2
   :end-before: END precondition2

Now this failure can no longer appear.

.. note::

   It's debatable whether the contract's behaviour is actually buggy
   or not. We decided to accept it, and exclude payments smaller than
   2 Ada from our tests. But of course, a *user* of the contract might
   attempt to make a payment of, say, 1 Ada--nothing prevents
   that. Such a user will see their wallet debited with 2 Ada, and may
   be surprised by that behaviour. Arguably the contract should
   explicitly *reject* payments below the minimum, rather than
   silently increase them. So this failing test does expose this
   issue.

.. _DesignIssue:

A third infelicity in the model, and a design issue
'''''''''''''''''''''''''''''''''''''''''''''''''''

Now that we have reasonable preconditions for each :hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action` in a
test, we can expect to see more interesting failures. And indeed, the
tests still fail, but now with a test case that combines payment and
redemption:

.. code-block:: text

   Prelude Spec.Tutorial.Escrow Test.QuickCheck Main> quickCheck prop_Escrow
   *** Failed! Assertion failed (after 8 tests and 5 shrinks):
   Actions
    [Pay (Wallet 4) 11,
     Pay (Wallet 5) 20,
     Redeem (Wallet 5)]
   Expected funds of W[5] to change by
     Value (Map [(,Map [("",-20000000)])])
   but they changed by
     Value (Map [(,Map [("",-19000000)])])
   a discrepancy of
     Value (Map [(,Map [("",1000000)])])

Here we made two payments, totalling 31 Ada, *which is exactly one Ada
more than the combined targets* (recall our targets are 10 Ada to
:hsobj:`Plutus.Contract.Test.w1` and 20 Ada to :hsobj:`Plutus.Contract.Test.w2`). Then :hsobj:`Plutus.Contract.Test.w5` redeemed the escrow, *and
ended up with 1 Ada too much* (last line). That extra Ada is, of
course, the extra unnecessary Ada that was paid to the script in the
previous action.

This raises the question: what *should* happen if an escrow holds more
funds than are needed to make the target payments? The designers of
this contract decided that any surplus should be paid to the wallet
submitting the :hsobj:`Plutus.Contracts.Tutorial.Escrow.redeem` transaction. Since this is part of the
intended behaviour of the contract, then our model has to reflect
it. We can do so with a small extension to the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.nextState` function
in the model:

.. literalinclude:: Escrow.hs
   :start-after: START nextState1
   :end-before: END nextState1

The extra code just computes the total contributions and the surplus,
and deposits the surplus in the calling wallet.

Now, at last, the tests pass!

.. code-block:: text

   Prelude Spec.Tutorial.Escrow Test.QuickCheck Main> quickCheck prop_Escrow
   +++ OK, passed 100 tests.

By default, quickCheck runs 100 tests, which is enough to reveal
easily-caught bugs such as those we have seen, but far too few to
catch really subtle issues. So at this point, it's wise to run many
more tests--the number is limited only by your patience and the speed
of the emulator:

.. code-block:: text

   Prelude Spec.Tutorial.Escrow Test.QuickCheck Main> quickCheck . withMaxSuccess 1000 $ prop_Escrow
   +++ OK, passed 1000 tests.

Analysing the distribution of tests
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Once tests are passing, then the framework displays statistics
collected from the running tests. These statistics give us important
information about the *effectiveness* of our tests; a risk with any
automated test case generation is that, since we do not usually
inspect the running tests, we may not notice if almost all of them
are trivial.

The contract model framework gathers some basic statistics by default,
and can be configured to gather more, but for now we just consider the
built-in ones. After each successful test run, we see a number of
tables, starting with a distribution of the actions performed by
tests:

.. code-block:: text

   Prelude Spec.Tutorial.Escrow Test.QuickCheck Main> quickCheck . withMaxSuccess 1000 $ prop_Escrow
   +++ OK, passed 1000 tests.

   Actions (25363 in total):
   75.894% Pay
   12.771% Redeem
   11.335% WaitUntil

Here we ran 1,000 tests, and as we see from the table, around 25,000
actions were generated. So, on average, each test case consisted of
around 25 actions.

Of those actions, three quarters were ``Pay`` actions, and 10-15% were
``Redeem``. This is not unreasonable--we decided when we wrote the
:hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action` generator to generate more payments than redemptions. The
remaining actions are ``WaitUntil`` actions, inserted by the
framework, which simply wait a number of slots to test for timing
dependence; we shall return to them later, but can ignore them for
now. Thus this distribution looks quite reasonable.

The second table that appears tells us how often a *generated*
:hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action` could not be included in a test, because its *precondition*
failed.

.. code-block:: text

   Actions rejected by precondition (360 in total):
   87.8% Redeem
   12.2% Pay

We can see that 360 actions--in addition to the 25,000 that were
included in tests--were generated, but *discarded* because their
preconditions were not true. This does represent wasted generation
effort, although rejecting 360 out of over 25,000 actions is not
really a serious problem--especially given that test case generation
is so very much faster than the emulator.

Nevertheless, we can see that the vast majority of rejected actions
were ``Redeem`` actions, and this is because a ``Redeem`` is not
allowed until sufficient payments have been made--but our generator
produces them anyway.

We can, of course, change this, to generate ``Redeem`` actions only
when redemption is actually possible:

.. literalinclude:: Escrow.hs
   :start-after: START arbitraryAction2
   :end-before: END arbitraryAction2

Measuring the distribution again after this change, we see that only
valid ``Redeem`` actions are now generated; the only discarded actions
are ``Pay`` actions.

.. code-block:: text

   Prelude Spec.Tutorial.Escrow Test.QuickCheck Main> quickCheck . withMaxSuccess 1000 $ prop_Escrow
   +++ OK, passed 1000 tests.

   Actions (25693 in total):
   76.717% Pay
   13.035% Redeem
   10.248% WaitUntil

   Actions rejected by precondition (650 in total):
   100.0% Pay

The main *disadvantage* of making this change is that it limits the
tests that *can* be generated, if the precondition of ``Redeem``
should be changed in the future. In particular, when we move on to
negative testing, then we will want to test invalid attempts to redeem
the escrow also. Once the generator is changed like this, then
relaxing the precondition is no longer enough to introduce invalid
calls. For this reason it could be preferable to *keep* the
possibility of generation invalid calls alongside the generator for
valid calls, but to assign the potentially-invalid generator a much
lower weight.

We will discuss the remaining tables in a later section.

Exercises
^^^^^^^^^

You can find the final version of the contract model discussed in this
section in ``Spec.Tutorial.Escrow1``, in the ``plutus-apps`` repo.

#. Try running the code in ``ghci``. You can do so by starting a
   ``nix`` shell, and starting ``ghci`` using

   .. code-block:: text

      cabal repl plutus-use-cases-test

   Then import QuickCheck and the contract model:

   .. code-block:: text

      import Test.QuickCheck
      import Spec.Tutorial.Escrow1

   and run tests using

   .. code-block:: text

      quickCheck prop_Escrow

   The tests should pass, and you should see tables showing the
   distribution of tested actions, and so on.

#. Try removing the preconditions for ``Pay`` and ``Redeem``, and
   reinserting them one by one. Run |quickCheck|_ after each change,
   and inspect the failing tests that are generated.

#. Try removing the line

   .. code-block:: text

      deposit w leftoverValue

   from the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.nextState` function, and verify that tests fail as
   expected.

#. Try removing one of the lines

   .. code-block:: text

      wait 1

   from the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.nextState` function (so that the model and the
   implementation get out of sync). What happens when you run tests?

#. This model does generate ``Pay`` actions that are discarded by the
   precondition. Adjust the generator so that invalid ``Pay`` actions
   are no longer generated, and run |quickCheck|_ to verify that this
   is no longer the case.


Parameterising Models and Dynamic Contract Instances
----------------------------------------------------

One of the unsatisfactory aspects of the tests developed in the
previous section is that they *always* pay 10 Ada to wallet 1, and 20
Ada to wallet 2. What if the contract only works for certain amounts,
or what if it only works with exactly two beneficiary wallets? Of
course, we would like to *generate* a random set of payment targets
for each test. Such a generator is easy to write:

.. literalinclude:: Escrow2.hs
   :start-after: START arbitraryTargets
   :end-before: END arbitraryTargets

but it is a little more intricate to make the model *use* these
generated targets.

There are two problems to overcome:

#. The generated targets are an important part of a test case, *so they
   must be included in the test case* somehow. But a test case is just
   a list of actions. So where do we put the targets?

#. The running contracts need to know what the targets are--but our
   model just contains a static list of contract instances in the test
   (:hsobj:`Plutus.Contract.Test.ContractModel.Interface.initialInstances`). How can we pass the generated targets to
   each running contract instance?


Solve these two problems, and we can test escrows with arbitrary payout targets. The techniques we learn will be applicable in many other situations.

Adding an initial action, and test case phases
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The first problem is quite easy to solve, in principle. The generated
payment targets are an important part of a test case, and a test case
is just a list of actions, therefore the generated payment targets
must be included in one or more of the actions. Quite simply, *any
generated data in a contract model test must be part of an action*. In
this case, we just decide that every test should begin with an
``Init`` action, that specifies the targets to be used in this test
case. So we must extend the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action` type to include ``Init``:

.. literalinclude:: Escrow2.hs
   :start-after: START Action
   :end-before: END Action

We must also ensure that ``Init`` actions *only* appear as the first
action of a test case, and that every test case starts with an
``Init`` action. We restrict the form of test cases using
preconditions, so this means that we must refine the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.precondition`
function so that the ``Init`` precondition only holds at the beginning
of a test case, and the other operations' preconditions only hold
*after* an ``Init`` has taken place.

However, the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.precondition` method is only given the action and the
contract state as parameters, which means in turn that we must be able
to tell whether or not we are at the beginning of the test case, just
from the model state. So we have to add a field to the model, to keep
track of where in a test case we are. In this simple case we could
just add a boolean ``initialised``, but we will be a little more
general and say that a test case is made up of a number of *phases*,
in this case just two, ``Initial`` and ``Running``:

.. literalinclude:: Escrow2.hs
   :start-after: START ModelState
   :end-before: END ModelState

Now we can specify that at the beginning of a test case we are in the
``Initial`` phase, and there are no targets:

.. literalinclude:: Escrow2.hs
   :start-after: START initialState
   :end-before: END initialState

and that when we model the ``Init`` action, we update both the phase
and the targets accordingly:

.. literalinclude:: Escrow2.hs
   :start-after: START nextState
   :end-before: END nextState

We have to specify how to perform an ``Init`` action also, but in this
case it exists only to initialise the model state with generated
targets, so performing it need not do anything:

.. literalinclude:: Escrow2.hs
   :start-after: START perform
   :end-before: END perform

Now we can add a precondition for ``Init``, and restrict the other
actions to the ``Running`` phase only:

.. literalinclude:: Escrow2.hs
   :start-after: START precondition
   :end-before: END precondition

It only remains to *generate* ``Init`` actions, using the
generator for targets that we saw above. We can take the phase into
account, and generate an ``Init`` action only at the start of the test
case, and other actions only in the ``Running`` phase.

.. literalinclude:: Escrow2.hs
   :start-after: START arbitraryAction
   :end-before: END arbitraryAction

.. note::

   Here we ensure that we always *generate* test cases that begin with
   ``Init``, but this is *not* enough to ensure that every test case
   we *run* begins with ``Init``. Remember that failed tests are
   always shrunk, and the first thing the shrinker will try is to
   discard the leading ``Init`` action (if that still results in a
   failing test, which it probably will). The only way to prevent
   shrinking from discarding the leading ``Init`` is for the
   *preconditions* to require it to be there. This is why we focussed
   on writing the preconditions first: they are more important.

As a matter of principle, when we write a generator, we also write a shrinker,
which just requires a one-line addition to the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.shrinkAction` function:

.. literalinclude:: Escrow2.hs
   :start-after: START shrinkAction
   :end-before: END shrinkAction

We cannot shrink wallets, which is why we can't simply apply
|shrink|_ to the list of targets, but using the |shrinkList|_
function from |Test.QuickCheck|_ we can easily write a shrinker that
will discard list elements and shrink the target values.

.. |Test.QuickCheck| replace:: ``Test.QuickCheck``
.. _Test.QuickCheck: https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html

.. |shrinkList| replace:: ``shrinkList``
.. _shrinkList: https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html#v:shrinkList

Dynamic contract instances
^^^^^^^^^^^^^^^^^^^^^^^^^^

At this point we can generate tests that begin by initialising the
escrow targets randomly, but we cannot yet run them successfully. If we try, we see failures like this:

.. code-block:: text

   *** Failed! Assertion failed (after 11 tests and 5 shrinks):
   Actions
    [Init [],
     Redeem (Wallet 1)]
   Contract instance log failed to validate:
   ...
   Slot 1: 00000000-0000-4000-8000-000000000000 {Wallet W[1]}:
             Contract instance stopped with error: RedeemFailed NotEnoughFundsAtAddress
   ...

Here we started a test with an empty list of targets, and tried to
redeem the escrow, but failed because there were 'not enough
funds'. Why not? Because *the contracts we are running still expect
the fixed targets* that we started with; we have not yet passed our generated
targets to the contract instances under test.

Recall the contract we are testing:

.. literalinclude:: Escrow.hs
   :start-after: START testContract
   :end-before: END testContract


It invokes the contract endpoints with the fixed set of :hsobj:`Plutus.Contracts.Tutorial.Escrow.EscrowParams`
we defined earlier. Clearly we need to parameterise the contract on
these :hsobj:`Plutus.Contracts.Tutorial.Escrow.EscrowParams` instead:

.. literalinclude:: Escrow2.hs
   :start-after: START testContract
   :end-before: END testContract

Now the question is: how do we pass this parameter to each ``testContract`` as we start them?

Recall the way we started contracts in the previous section. We
defined the contracts to start at the beginning of a test in the
:hsobj:`Plutus.Contract.Test.ContractModel.Interface.initialInstances` method:

.. literalinclude:: Escrow.hs
   :start-after: START initialInstances
   :end-before: END initialInstances



Each contract is specified by a :hsobj:`Plutus.Contract.Test.ContractModel.Interface.StartContract`, containing not only
a contract instance key, but also a *parameter*--in this case ``()``,
since we did not need to pass any generated values to
``testContract``. Now we do need to, so we must replace that ``()``
with escrow parameters generated from our payment targets. Moreover,
we can no longer start the contracts at the beginning of the test--we
must see the ``Init`` action first, so that we know what the generated
targets are. To do so, we redefine

.. literalinclude:: Escrow2.hs
   :start-after: START initialInstances
   :end-before: END initialInstances

and instead add a definition of the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.startInstances` method:

.. literalinclude:: Escrow2.hs
   :start-after: START startInstances
   :end-before: END startInstances

where the escrow parameters are now constructed from our generated targets:

.. literalinclude:: Escrow2.hs
   :start-after: START escrowParams
   :end-before: END escrowParams

The effect of this is to start the contracts *just before* the
``Init`` action; in fact, using this mechanism, we can start contracts
dynamically at any point in a test case.

.. note::

   We should be careful to avoid reusing the same contract instance
   key more than once, though, since this may lead to confusing
   results.

   You may wonder why we don't simply start new contract instances in the
   :hsobj:`Plutus.Contract.Test.ContractModel.Interface.perform` method instead. The answer is the framework needs to track
   the running contract instances, and using :hsobj:`Plutus.Contract.Test.ContractModel.Interface.startInstances` makes
   this explicit.

The :hsobj:`Plutus.Contract.Test.ContractModel.Interface.StartContract` just specifies the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.ContractInstanceKey` to be
started; we define the actual contract to start in the
:hsobj:`Plutus.Contract.Test.ContractModel.Interface.instanceContract` method, *which receives the contract parameter*
from :hsobj:`Plutus.Contract.Test.ContractModel.Interface.StartContract` as its last argument. So we can just define

.. literalinclude:: Escrow2.hs
   :start-after: START instanceContract
   :end-before: END instanceContract

and our work is (almost) done. The last step is just to update the
*type* of ``WalletKey``, since it includes the type of the parameter
that :hsobj:`Plutus.Contract.Test.ContractModel.Interface.StartContract` accepts.

.. literalinclude:: Escrow2.hs
   :start-after: START ContractInstanceKey
   :end-before: END ContractInstanceKey

Now, at last, our extended model is complete.

Running our extended tests; another design issue
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can now run our new tests, and, as so often happens when the scope
of QuickCheck tests is extended, they do not pass. Here is an example
of a failure:

.. code-block:: text

   Actions
    [Init [(Wallet 5,0)],
     Redeem (Wallet 4)]
   Expected funds of W[4] to change by
     Value (Map [])
   but they changed by
     Value (Map [(,Map [("",-2000000)])])
   a discrepancy of
     Value (Map [(,Map [("",-2000000)])])
   Expected funds of W[5] to change by
     Value (Map [])
   but they changed by
     Value (Map [(,Map [("",2000000)])])
   a discrepancy of
     Value (Map [(,Map [("",2000000)])])
   Test failed.

In this case the generated target just specifies that wallet 5 should
receive 0 Ada--a slightly odd target, perhaps, but not obviously
invalid. Since the total of all targets is 0 Ada, then the target is
already met, and wallet 4 attempts to redeem the escrow. We might
expect the effect to be a no-op--and this is what our model
predicts--but it is not what happens. Instead, *wallet 4 pays two Ada
to wallet 5*!

The reason this happens is the blockchain rule that every transaction
output must contain at least 2 Ada. When wallet 4 attempts to redeem
the escrow, then the off-chain code attempts to create a transaction
with an output paying 0 Ada to wallet 5, but that is increased to 2
Ada to make the transaction valid. Then when the transaction is
balanced, the 2 Ada is taken from the submitting wallet.

Is this a bug in the contract? It is certainly an inconsistency with
the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.nextState` function in the model, and we could modify that
function to reflect the *actual* transfers of Ada that the contract
performs. But these transfers were surely not intentional: a more
reasonable approach is to say that target payments that are too small
to be accepted by the blockchain are invalid; such targets should not
be chosen.

We can make our tests pass by tightening the precondition of ``Init``
so that targets below the minimum are not accepted:

.. literalinclude:: Escrow2.hs
   :start-after: START tightprecondition
   :end-before: END tightprecondition

This demonstrates that the contract works as expected, provided we
*don't* specify targets less than the minimum, but nothing prevents a
*user* of the contract from specifying such targets--and we know that
the contract code will accept them, and deliver surprising results in
those cases. Arguably *all* the contract endpoints should check that
the specified targets are valid, and raise an error if they are
not. This would prevent the *creation* of invalid escrows, rather than
generating unexpected behaviour when they are redeemed.

Thus these failing tests *do* suggest a way in which the contract
implementation can be improved, even if the failing cases are fairly
unlikely in practice.

.. note::

   QuickCheck was able to find this bug because our *generator* for
   target payments includes invalid values; we chose values in the
   range 1 to 31, where 1 is invalid (and shrinking reduced the 1 to a
   0 in the failing case that was reported). It is a good thing we did
   not ensure, from the start, that only valid target values could be
   generated--had we done so, we would not have discovered this
   anomalous behaviour.

   In general, it is a good idea for generators to produce, at least
   occasionally, every kind of input that a user can actually supply,
   even if some of them are invalid (and may be filtered out by
   preconditions). Doing so enables this kind of strange behaviour to
   be discovered.

Exercises
^^^^^^^^^

#. You will find the code presented here in ``Spec.Tutorial.Escrow2``,
   with the exception of the last precondition we discussed for
   ``Init``. Run the tests using

   .. code-block:: text

                   quickCheck prop_Escrow

   and make sure you understand how they fail.

#. Make your own copy of the code, and add the tighter precondition
   for ``Init``. Verify that the tests then pass.

#. An alternative explanation for the problem might have been that a
   target of *zero* should not be allowed (and perhaps the contract
   implementation should interpret a target of zero by not creating a
   transaction output at all). *Change the precondition* of ``Init``
   so that it only excludes targets of zero, rather than any target
   below the minimum. Verify that the tests still fail, and make sure
   you understand the (slightly more complex) failure.

#. There are quite a few steps involved in introducing these
   dynamically chosen targets. You can practice these steps by taking
   the code from ``Spec.Tutorial.Escrow1``, which uses fixed targets,
   and following the steps outlined in this tutorial to turn it into a
   copy of ``Spec.Tutorial.Escrow2``.



Testing "No Locked Funds" with Dynamic Logic
--------------------------------------------

So far, we have tested that a contract's actual transfers of tokens
are consistent with the model. That is, *nothing goes wrong*--or to
put it bluntly, nobody steals your money. This is an example of a
*safety property*. But when we use smart contracts, this is not the
only kind of property we care about. Very often, we *also* want to be
certain that we can eventually reach some kind of *goal* state--an
example of a *liveness property*. In particular, it would be bad if
tokens were to be trapped in a contract for ever, with no possibility
of recovering them. The Cardano model certainly allows this... imagine
a UTXO whose verifier always returns ``False``... and so it is our
responsibility to ensure that contracts do not fall into this
trap. Not only does nothing go wrong, but *something good is always
possible*. Not only does no-one steal your money, but you can always
recover it yourself.

We call these properties "no locked funds" properties, because that is
usually what we want to test: that we can always reach a state in
which all tokens have been recovered from the contracts under test. Of
course, there is no *general* way to recover tokens held by a
contract, so we cannot expect QuickCheck to find a way to reach this
goal automatically; instead, we *specify a strategy* for recovering
funds, and what we test is that the given strategy always works.

Writing and testing properties using Dynamic Logic
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We specify this kind of property using *dynamic logic*.
This part of the contract testing framework is inspired
by `dynamic logic for reasoning about programs
<http://en.wikipedia.org/wiki/Dynamic_logic_(modal_logic)>`_, but it
can be thought of just as a way of writing *test scenarios*, in
which we mix random generation, explicit actions, and assertions. We
write such scenarios in the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.DL` monad; for example, here is a scenario
that first performs a random sequence of actions, then invokes a
finishing strategy, and finally asserts that no tokens remain locked
in contracts.

.. literalinclude:: Escrow3.hs
   :start-after: START finishEscrow
   :end-before: END finishEscrow

Here :hsobj:`Plutus.Contract.Test.ContractModel.Interface.assertModel` lets us include an assertion about the contract
model state, :hsobj:`Plutus.Contract.Test.ContractModel.Interface.lockedValue` is a function provided by the framework
that computes the total value held by contracts, and :hsobj:`Plutus.Contract.Test.ContractModel.Interface.symIsZero`
checks that this is zero. The value is returned here as a
:hsobj:`Plutus.Contract.Test.ContractModel.Symbolics.SymValue`, but for now it can be thought of just as a normal
Plutus :hsobj:`Ledger.Value` with an extra type wrapper.

This scenario just tests that the given finishing strategy always
succeeds in recovering all tokens from contracts, no matter what
actions have been performed beforehand. The finishing strategy itself
is written in the same monad. For example, if we think we should use a
``Redeem`` action to recover the tokens, then we can define

.. literalinclude:: Escrow3.hs
   :start-after: START badFinishingStrategy
   :end-before: END badFinishingStrategy

Of course, since the strategy must work in any state, including the
initial one, then we do have to check that the escrow has been
initialised before we attempt to ``Redeem``.

.. note::

   These test scenarios are very flexible, and can be used for other
   purposes too. For example, we could write a test scenario that fixes
   the escrow targets, thus undoing the generalization we made in the previous section:

   .. literalinclude:: Escrow3.hs
      :start-after: START fixedTargets
      :end-before: END fixedTargets

   Note that generated actions are always *appropriate for the current
   state*, so here :hsobj:`Plutus.Contract.Test.ContractModel.Interface.anyActions_` will pick up generating the test case
   from the point after the escrow targets are initialised.

   We can use dynamic logic to express everything from unit tests to full
   random generation (by just specifying :hsobj:`Plutus.Contract.Test.ContractModel.Interface.anyActions_` as the
   scenario). But for now, we focus on testing "no locked funds" properties.

Now, dynamic logic just specifies a *generator* for tests to
perform; we still need to specify *how* to perform those
tests. Usually, we just reuse the existing property we have already
written, which runs the test case on the emulator and performs the
usual checks. In this case, we can define

.. literalinclude:: Escrow3.hs
   :start-after: START prop_FinishEscrow
   :end-before: END prop_FinishEscrow


Then we can run the tests by passing the property to |quickCheck|_, as usual:

   .. code-block:: text

      > quickCheck prop_FinishEscrow
      *** Failed! Falsified (after 1 test and 3 shrinks):
      BadPrecondition
        [Do $ Init [(Wallet 2,2)]]
        [Action (Redeem (Wallet 1))]
        (EscrowModel {_contributions = fromList [],
                      _targets = fromList [(Wallet 2,Value (Map [(,Map [("",2000000)])]))],
                      _phase = Running})

      BadPrecondition
      [Do $ Var 0 := Init [(Wallet 2,2)]]
      Some (Redeem (Wallet 1))

The property fails, which is not surprising: our "finishing strategy"
is quite simplistic, and not yet expected to work. But let us inspect
the error message. The test failed because of a bad precondition,
after running the sequence

  .. code-block:: text

     Init [(Wallet 2,2)]

So we set up a target to pay wallet 2 a sum of 2 Ada. Then we tried to
apply our finishing strategy, which is just for wallet 1 to issue a
``Redeem`` request:

  .. code-block:: text

     Redeem (Wallet 1)

This wasn't possible, because the precondition of ``Redeem`` wasn't
satisfied. The message also shows us the model state--we have set up
the escrow targets successfully, but there are no contributions, and
the ``Redeem`` precondition says that the contributions must cover the
targets before ``Redeem`` is possible. So of course, it doesn't work.

But the counterexample does show us what we need to do to *make*
``Redeem`` possible: we need to pay in sufficient contributions to
cover the targets. So that suggests a refined finishing strategy:


.. literalinclude:: Escrow3.hs
   :start-after: START finishingStrategy
   :end-before: END finishingStrategy

We read the contributions and targets from the contract state, compute
the remaining deficit, and if the deficit is positive, then we make a
payment to cover it. After this, a ``Redeem`` should be
successful. And indeed, testing the property passes: this finishing
strategy works.

  .. code-block:: text

   >  quickCheck . withMaxSuccess 1000 $ prop_FinishEscrow
   +++ OK, passed 1000 tests.

   Actions (51925 in total):
   73.483% Pay
   14.278% Redeem
   10.315% WaitUntil
    1.924% Init

.. _StrictRedeem:

Digression: revisiting a design decision
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In section :ref:`DesignIssue` above, we discussed the situation in
which contributors pay in *more* to the escrow than is needed to meet
the targets. The actual contract allows that, and so do we in our
model; as a consequence we had to *specify* where the surplus funds
end up on redemption (in the wallet that invokes ``Redeem``). But
there is another approach we could have taken: we could simply have
said that ``Redeem`` *requires* the contributions and targets to match
exactly, by strengthening the precondition:

.. literalinclude:: Escrow3.hs
   :start-after: START strongPrecondition
   :end-before: END strongPrecondition

This does make ``prop_Escrow`` pass:

  .. code-block:: text

   > quickCheck prop_Escrow
   +++ OK, passed 100 tests.

   Actions (2845 in total):
   82.81% Pay
   13.78% WaitUntil
    3.30% Init
    0.11% Redeem

   Actions rejected by precondition (870 in total):
   88.3% Redeem
   10.8% Pay
    0.9% Init

But should we be satisfied with this? There are warning signs in the
statistics that QuickCheck collects:

#. We have tested ``Redeem`` an extremely small number of times.
#. A high proportion of generated ``Redeem`` actions were *discarded* by the precondition.

The explanation for this is that we can now only include ``Redeem`` in
a test case if the previous (random) payments have hit the target
*exactly*, and this is very unlikely. Moreover, once we have overshot
the target, then further random payments cannot help.

We could add a stronger *precondition* to ``Pay``, that forbids
payments taking us over the target, and that would result in a better
distribution of actions. But it is not a *realistic* solution, because
at the end of the day, there is no way to *prevent* someone making a
payment to a script on the Cardano blockchain. *Making a payment to a
contract does not require the contract's approval*.

So there is a problem here, but when we test ``prop_Escrow``, then it
is revealed only by careful inspection of the generated
statistics--the property does not *fail*.

On the other hand, when we test ``prop_FinishEscrow``, then it fails immediately:

  .. code-block:: text

   > quickCheck prop_FinishEscrow
   *** Failed! Falsified (after 5 tests and 6 shrinks):
   BadPrecondition
     [Do $ Init [],
      Do $ Pay (Wallet 2) 2]
     [Action (Redeem (Wallet 1))]
     (EscrowModel {_contributions = fromList [(Wallet 2,Value (Map [(,Map [("",2000000)])]))],
                   _targets = fromList [],
                   _phase = Running})

   BadPrecondition
   [Do $ Var 0 := Init [],Do $ Var 3 := Pay (Wallet 2) 2]
   Some (Redeem (Wallet 1))

The counterexample sets up an escrow with an empty list of targets
(which may seem odd, but is allowed, and tells us that no particular
targets are *needed* to make the property fail). Then it makes a
payment to the escrow, thus overshooting the targets. Finally, we try
to use the given finishing strategy--which just attempts to use
``Redeem``, and fails because the strong precondition we wrote does
not allow it.

In this case, not only does the given finishing strategy fail, but the
bug cannot be fixed: *there is no possible finishing strategy that
works*. Once we have overshot the targets, there is no way to return
to a state in which ``Redeem`` is possible! And that is why the
contract authors did *not* follow this path: had they done so, then an
attacker would be able to 'brick' an escrow contract just by making an
unexpected payment to it.

Fair's fair: Unilateral strategies
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We saw above how to test that a finishing strategy succeeds in
recovering all the tokens. But not all strategies are created
equal. For example, suppose you use an escrow contract to buy an
NFT. You place your funds in the escrow, but before the seller can
place the NFT there, they get a better offer. Now the seller will
never place the NFT in the escrow--and neither can the buyer--and so
the buyer's funds *will* be locked for ever, even though there is a
way (using the NFT) to recover them.

This little story shows that there is a need for each wallet to be
able to recover their "fair share" of the funds in the contract,
without any other wallet's cooperation. And the contract model
framework provides a way of testing this too.

The idea is to provide *two* strategies, one that recovers all the
tokens from contracts, and is also interpreted to define each wallet's
"fair share", and a second strategy *that can be followed by any
single wallet*, and recovers that wallet's tokens. We call this kind
of strategy a *unilateral* strategy; it is defined in the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.DL` monad
in just the same way as the strategies we saw earlier, but only a
single wallet is allowed to perform actions. Indeed, this is why we
gave ``finishingStrategy`` a wallet as a parameter: it defines the
unilateral strategy for that wallet. Since the strategy uses
``Redeem``, which actually pays out *all* the targets, then we can
reuse it as the general strategy too, just by choosing a wallet to
perform it (and we chose wallet 1 above).

The framework lets us package the general and unilateral strategies
together, into a "no locked funds proof":

.. literalinclude:: Escrow3.hs
   :start-after: START noLockProof
   :end-before: END noLockProof

.. note::

   There are other components in a :hsobj:`Plutus.Contract.Test.ContractModel.Interface.NoLockedFundsProof`, which we
   will see later; we can ignore them for now, but we do need to take
   suitable default values from :hsobj:`Plutus.Contract.Test.ContractModel.Interface.defaultNLFP` in the definition
   above.

and we can test them together using :hsobj:`Plutus.Contract.Test.ContractModel.Interface.checkNoLockedFundsProof`

.. literalinclude:: Escrow3.hs
   :start-after: START prop_NoLockedFunds
   :end-before: END prop_NoLockedFunds

.. code-block:: text

      > quickCheck prop_NoLockedFunds
      *** Failed! Falsified (after 1 test and 5 shrinks):
      DLScript
        [Do $ Init [(Wallet 4,2)]]

      Unilateral strategy for Wallet 4 should have gotten it at least
        SymValue {symValMap = fromList [], actualValPart = Value (Map [(,Map [("",2000000)])])}
      but it got
        SymValue {symValMap = fromList [], actualValPart = Value (Map [])}

The property actually fails, because if all we do is create a target
that wallet 4 should receive 2 Ada, then wallet 4's unilateral
strategy is unable to recover that--even though, when wallet 1 follows
the strategy, then wallet 4 does receive the money.

What happens here is that the *general* strategy, which is just the
same strategy followed by wallet 1, *does* pay out to wallet 4, and so
we *define* wallet 4's "fair share" to be 2 Ada. But this isn't really
right, because since no Ada have been paid into the contract, then
there are no tokens to disburse. Indeed, if anything, the "general"
strategy is *unfair* to wallet 1, which has to stump up 2 Ada in this
situation so that the escrow can be redeemed. So this test failure
does reveal a fairness problem, even if the victim is really wallet 1
rather than wallet 4.

We will see how to fix this problem in the next section. In the
meantime, to summarize, defining a :hsobj:`Plutus.Contract.Test.ContractModel.Interface.NoLockedFundsProof` requires us

#. to define a general strategy that can recover *all* the tokens from
   the contracts under test, and moreover implies a *fair share* of
   the tokens for each wallet *in any state* (for example, a fair
   share of the profits-so-far of any trading contract),

#. to define a *unilateral strategy* for each wallet, that can recover
   that wallet's fair share of the tokens from any state, without
   cooperation from any other wallet.


Fixing the contract: refunds
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The fundamental problem with the finishing strategy we have developed
so far, is that in order to recover tokens already held by the
contract, we may need to pay in even more tokens! This seems a poor
design. It would make far more sense, in the event that the contract
is not followed to completion, to *refund* contributions to the
wallets that made them. And indeed, the actual implementation of the
contract supports a :hsobj:`Plutus.Contracts.Tutorial.Escrow.refund` endpoint as well.

To add refunds to our model, we need to add a new action

.. literalinclude:: Escrow3.hs
   :start-after: START EscrowModel
   :end-before: END EscrowModel

and add it to :hsobj:`Plutus.Contract.Test.ContractModel.Interface.nextState`, :hsobj:`Plutus.Contract.Test.ContractModel.Interface.precondition`, :hsobj:`Plutus.Contract.Test.ContractModel.Interface.perform`, and :hsobj:`Plutus.Contract.Test.ContractModel.Interface.arbitraryAction`:

.. literalinclude:: Escrow3.hs
   :start-after: START RefundModel
   :end-before: END RefundModel

(In the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.nextState` clause, the first line uses a more complex lens
to extract the contributions, select the value for wallet ``w``, if
present, and then pass the resulting ``Maybe Value`` to |fold|_, thus
returning zero if there was no contribution, and the value itself if
there was). We also have to extend the ``testContract`` to include the
refund endpoint:

.. literalinclude:: Escrow3.hs
   :start-after: START testContract
   :end-before: END testContract


With these additions, ``prop_Escrow`` still passes, but
now tests refunds as well:

.. code-block:: text

   > quickCheck prop_Escrow
   +++ OK, passed 100 tests.

   Actions (2625 in total):
   66.44% Pay
   12.46% WaitUntil
    9.64% Redeem
    7.96% Refund
    3.50% Init

   Actions rejected by precondition (478 in total):
   85.8% Refund
   12.6% Pay
    1.7% Init

We can see that ``Refund`` is tested almost as often as ``Redeem``,
although many refunds are rejected by the precondition (which requires
that there actually *is* a contribution to refund). This isn't a big
deal, though, because the overall proportion of rejected actions is
low (15%), and sufficiently many ``Refund`` actions are being tested.

The payoff, though, is that we can now define a far better finishing
strategy: the general strategy will just refund all the contributions,
and the unilateral strategies will claim a refund for the
wallet concerned.

.. literalinclude:: Escrow3.hs
   :start-after: START BetterStrategies
   :end-before: END BetterStrategies

.. note::

   Here we use :hsobj:`Plutus.Contract.Test.ContractModel.Interface.monitor` to gather statistics on the number of
   wallets receiving refunds during the finishing strategy, just to
   make sure, for example, that it is not always zero. We place such
   monitoring in the *general* strategy, not the wallet-specific ones,
   because the general strategy is invoked exactly once per test,
   while the wallet-specific ones may be invoked a variable--and
   unpredictable--number of times. This makes statistics gathered in
   the wallet-specific strategies harder to interpret.

We put these strategies together into a :hsobj:`Plutus.Contract.Test.ContractModel.Interface.NoLockedFundsProof`:

.. literalinclude:: Escrow3.hs
   :start-after: START BetterNoLockProof
   :end-before: END BetterNoLockProof

and run tests:

.. code-block:: text

   > quickCheck prop_NoLockedFunds
   +++ OK, passed 100 tests.

   Actions (31076 in total):
   65.211% Pay
   11.794% WaitUntil
   10.117% Redeem
    9.506% Refund
    1.847% Init
    1.525% Unilateral

   Refunded wallets (100 in total):
   30% 2
   23% 1
   17% 4
   16% 3
   13% 0
    1% 5

Now the tests pass--each wallet can indeed recover its own fair share
of tokens--and moreover we test each action fairly often, and the
number of refunded wallets has a reasonable-looking distribution.

Exercises
^^^^^^^^^
You will find the code presented in this section in ``Spec.Tutorial.Escrow3``\.

#. Strengthen the precondition of ``Redeem`` to require the
   contributions and targets to match exactly, as discussed in
   :ref:`StrictRedeem`. Verify that ``prop_Escrow`` passes and
   ``prop_FinishEscrow`` fails. Now, *add a precondition to* ``Pay``
   to disallow payments that take the contributions over the target.

   #. Test ``prop_Escrow``, and make sure it passes; have you achieved
      a better distribution of actions in tests?

   #. Test ``prop_FinishEscrow``; does it pass now?

#. The code provided uses the poor finishing strategy based on
   ``Redeem``. Verify that ``prop_NoLockedFunds`` fails, and replace
   the strategy with the better one described above (you will find the
   code in comments in the file). Verify that ``prop_NoLockedFunds``
   passes now.

   Do not be surprised if testing ``prop_NoLockedFunds`` is
   considerably slower than testing ``prop_FinishEscrow``. The latter
   runs the emulator only once per test, while the former must run it
   repeatedly to test each wallet's unilateral strategy.

#. Sometimes a wallet which is targetted to *receive* funds might do
   better to complete the contributions and redeem the escrow, rather
   than refund its own contribution. Implement this idea as a
   per-wallet strategy, and see whether ``prop_NoLockedFunds`` still
   passes. Add a call of :hsobj:`Plutus.Contract.Test.ContractModel.Interface.monitor` to your strategy to gather
   statistics on how often ``Redeem`` is used instead of ``Refund``.


Taking Time into Account
------------------------

In the last section we added refunds to our tests; now a client can
pay into an escrow, and claim a refund of their contribution
freely--but this doesn't really correspond to the intention of an
escrow contract. In reality, an escrow contract should have a deadline
for payments and redemption, with refunds permitted only after the
deadline has passed. In fact, the *real* escrow contract, in
:hsmod:`Plutus.Contracts.Escrow`, provides such a deadline: the main
difference between this and the simplified contract we have tested so
far, :hsmod:`Plutus.Contracts.Tutorial.Escrow`, is that the latter omits
the deadline and associated checks.

In this section, we'll switch to testing the real contract, which we
can achieve just by changing the import in our model to be the real
contract. (As usual, you can find the code presented in this section
in ``Spec.Tutorial.Escrow4``\).

Slots and POSIXTime
^^^^^^^^^^^^^^^^^^^

Just changing the import leads to a compiler warning: the
:hsobj:`Plutus.Contracts.Escrow.EscrowParams` type, which is passed to the contract under test, has
a new field :hsobj:`Plutus.Contracts.Escrow.escrowDeadline`, and so far, our code does not
initialise it. We will generate the deadlines, so that they vary from
test to test, but there is a slight mismatch to overcome first. In a
contract model we measure time in *slots*, but the :hsobj:`Plutus.Contracts.Escrow.escrowDeadline`
field is not a slot number, it is a :hsobj:`Plutus.V1.Ledger.Time.POSIXTime`. So while we shall
generate the deadline as a slot number (for convenience in the model),
we must convert it to a :hsobj:`Plutus.V1.Ledger.Time.POSIXTime` before we can pass it to the
contract under test.

To do so, we need to know when slot 0 happens in POSIX time, and how
long the duration of each slot is. These are defined in a
:hsobj:`Cardano.Node.Emulator.TimeSlot.SlotConfig`, a type defined in :hsmod:`Cardano.Node.Emulator.TimeSlot`. In principle
the slot configuration might vary, but we will use the default values
for testing (by using |def|_ from |Data.Default|_ as our
configuration. Putting all this together, we can add a deadline to our
:hsobj:`Plutus.Contracts.Escrow.EscrowParams` as follows:

.. |Data.Default| replace:: ``Data.Default``
.. _Data.Default: https://hackage.haskell.org/package/data-default-0.7.1.1/docs/Data-Default.html

.. |def| replace:: ``def``
.. _def: https://hackage.haskell.org/package/data-default-0.7.1.1/docs/Data-Default.html#v:def

.. literalinclude:: Escrow4.hs
   :start-after: START escrowParams
   :end-before: END escrowParams


.. note::

   If you are familiar with the |Clock.POSIXTime|_ type from
   |Data.Time.Clock.POSIX|_, then beware that *this is not the same
   type*. That type has a resolution of picoseconds, while Plutus uses
   its own :hsobj:`Plutus.V1.Ledger.Time.POSIXTime` type with a resolution of milliseconds.

.. |Clock.POSIXTime| replace:: ``POSIXTime``
.. _Clock.POSIXTime: https://hackage.haskell.org/package/time-1.13/docs/Data-Time-Clock-POSIX.html#t:POSIXTime

.. |Data.Time.Clock.POSIX| replace:: ``Data.Time.Clock.POSIX``
.. _Data.Time.Clock.POSIX: https://hackage.haskell.org/package/time-1.13/docs/Data-Time-Clock-POSIX.html

Initialising the deadline
^^^^^^^^^^^^^^^^^^^^^^^^^

The deadline, like the escrow targets, is fixed for each test, so it
makes sense to add the deadline as a new field to the ``Init``
action--recall that it is the ``Init`` action that starts the contract
instances under test, and so must supply the deadline as part of the
:hsobj:`Plutus.Contracts.Escrow.EscrowParams`. So we add the deadline slot to this action

.. literalinclude:: Escrow4.hs
   :start-after: START Action
   :end-before: END Action

and pass it to the contracts in the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.startInstances` method:

.. literalinclude:: Escrow4.hs
   :start-after: START startInstances
   :end-before: END startInstances

Just as we record the escrow targets in the model state, so we will
need to include the deadline as part of the model, so we extend our
model type

.. literalinclude:: Escrow4.hs
   :start-after: START EscrowModel
   :end-before: END EscrowModel

and record the deadline in our model state transition:

.. literalinclude:: Escrow4.hs
   :start-after: START nextState
   :end-before: END nextState

It just remains to generate deadline slots (we choose positive
integers), and shrink them (by reusing integer shrinking):

.. literalinclude:: Escrow4.hs
   :start-after: START arbitraryAction
   :end-before: END arbitraryAction

.. literalinclude:: Escrow4.hs
   :start-after: START shrinkAction
   :end-before: END shrinkAction

Now we are ready to run tests.

 .. _Timing:

Modelling the passage of time
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can now run tests, but they do not pass:

.. code-block:: text

   > quickCheck prop_Escrow
   *** Failed! Assertion failed (after 5 tests and 7 shrinks):
   Actions
    [Init (Slot {getSlot = 0}) [],
     Pay (Wallet 1) 2]
   Expected funds of W[1] to change by
     Value (Map [(,Map [("",-2000000)])])
   but they did not change
   Test failed.
   Emulator log:
   [INFO] Slot 0: TxnValidate ee3a44b98e0325e19bc6be1e6f25cdb269301666a3473758296e96cd7ea9a851
   [INFO] Slot 1: 00000000-0000-4000-8000-000000000000 {Wallet W[1]}:
                    Contract instance started
   [INFO] Slot 1: 00000000-0000-4000-8000-000000000001 {Wallet W[2]}:
                    Contract instance started
   ...

We tried to pay 2 Ada from wallet 1, but the payment did not take
effect. Notice that the generated deadline is slot zero, though; in
other words, the deadline passed before we started the test. This
might seem surprising, since we *generated* the deadline as a positive
integer (and zero does not count as positive), but it is the result of
shrinking. If we don't want to test a deadline of slot zero, then we
must strengthen the precondition of ``Init`` to prevent it.

Noting that the contract instances do not start until slot one, let us
require the deadline slot to be greater than that--at least slot
two. When we add this to the precondition then tests still fail, but
the shrunk counterexample is different:

.. code-block:: text

   > quickCheck prop_Escrow
   *** Failed! Assertion failed (after 2 tests and 5 shrinks):
   Actions
    [Init (Slot {getSlot = 2}) [],
     WaitUntil (Slot {getSlot = 2}),
     Pay (Wallet 3) 2]
   Expected funds of W[3] to change by
     Value (Map [(,Map [("",-2000000)])])
   but they did not change
   Test failed.

This test case makes the problem easier to see: it

#. first, initializes the deadline to slot 2

#. then, *waits until* slot 2,

#. and finally, attempts a payment, which does not go through.

The second action, ``WaitUntil``, is one we have not seen in
counterexamples previously; it only appears when *timing is important*
to provoke a failure. In this case it's now clear what the problem is:
*the contract does not allow payments after the deadline*. So the next
step is to encode this in our model.

.. note::

   ``WaitUntil`` actions are inserted automatically into test cases by
   the framework, to explore timing dependence. It is *possible* to
   control the probability of a ``WaitUntil`` action, and the
   distribution of the slots that we wait for, but it is often not
   *necessary*--the default behaviour is often good enough.

The contract model framework automatically keeps track of the current
slot number for us, so we *could* write a precondition for ``Pay``
that refers explicitly to the slot number. However, all that really
matters is *whether or not the deadline has passed*--and probably
other parts of the model will depend on this too. So it is simpler to
check for this in one place, and then just refer to it elsewhere in
the model.

Now we can benefit from our choice earlier to introduce a ``phase``
field in the model: hitherto it has only distinguished initialization
from running the test, but now we can add a new phase: ``Refunding``

.. literalinclude:: Escrow4.hs
   :start-after: START Phase
   :end-before: END Phase

The idea is that when the deadline passes, we move into the
``Refunding`` phase, and we can refer to the current phase in
preconditions. In fact, our preconditions *already* refer to the
phase, so with this change then ``Pay`` and ``Redeem`` will be
restricted to take place *before* the deadline. All we have to do is
to adjust the precondition for ``Refund``, which should of course be
restricted to *after* the deadline:

.. literalinclude:: Escrow4.hs
   :start-after: START precondition
   :end-before: END precondition

One question remains: *where do we change the phase?* Changing the
phase changes the model state, but not in response to an :hsobj:`Plutus.Contract.Test.ContractModel.Interface.Action`:
it doesn't matter whether or not an action is performed on the
deadline, the phase must change anyway. This means that *we cannot
change the phase in the* :hsobj:`Plutus.Contract.Test.ContractModel.Interface.nextState` *function*, because this is
invoked only when actions are performed. We need to be able to *change
the contract state in response to the passage of time*. We can do this
by defining the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.nextReactiveState` method of the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.ContractModel`
class.

This method is called every time the slot number advances in the model
(although not necessarily every slot--slot numbers can jump during a
test). In this case all we need to do is compare the new slot number
with the deadline, and move to the ``Refunding`` phase if appropriate:

.. literalinclude:: Escrow4.hs
   :start-after: START nextReactiveState
   :end-before: END nextReactiveState

Now ``prop_Escrow`` passes.

Monitoring and the distribution of tests
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Testing ``prop_Escrow`` generates some interesting statistics:

.. code-block:: text

   > quickCheck prop_Escrow
   +++ OK, passed 100 tests.

   Actions (2291 in total):
   62.03% WaitUntil
   27.37% Pay
    3.71% Redeem
    3.62% Init
    3.27% Refund

   Actions rejected by precondition (11626 in total):
   70.437% Pay
   23.757% Refund
    5.746% Redeem
    0.060% Init

In comparison with previous versions of this property, we can see from
the first table that there are *many* more ``WaitUntil`` actions in
these tests (previously they were around 10% of the tested
actions). Moreover, we can see that many more generated actions were
rejected by their precondition: we rejected over 11,000 actions, while
generating 2291 that were included in tests. Rejecting so many actions
is undesirable: not only does it waste testing time, but there is a
risk that the *distribution* of accepted actions is quite different
from that of generated actions, which can lead to ineffective testing.

But why do we see this behaviour? It is because *once the deadline has
passed, then neither* ``Pay`` *nor* ``Redeem`` *is possible*; when we
generate these actions, then they will *always* be rejected by their
preconditions. Moreover, *after the deadline then we can* ``Refund``
*each wallet at most once*. Once the deadline has passed, and all the
contributions have been refunded, then the preconditions allow no
further actions--except ``WaitUntil``. And so, test case generation
will choose ``WaitUntil``, over and over again, and this is why so
many of them appear in our tests.

The following tables tell us more about the passage of time in our tests:

.. code-block:: text

   Wait interval (1421 in total):
   28.85% <10
   25.83% 10-19
   23.15% 20-29
   15.76% 30-39
    5.77% 40-49
    0.63% 50-59

   Wait until (1421 in total):
   14.07% 100-199
   12.03% 1000-1999
    9.29% 200-299
    8.94% 300-399
    7.67% 400-499
    ...
    2.32% 2000-2999
    ...

The first table shows us *how long* we waited at each individual
occurrence of ``WaitUntil``: mostly under 30 slots, but up to 59 slots
at a maximum. The second table shows us which slot numbers we waited
until: we can see that many tests ran for several hundred slots, and
indeed, some ran for over 2000 slots.

Luckily, waiting is cheap, but since we are performing fewer useful
actions in each test, then we should probably run more tests overall
for the same level of confidence in our code.

No locked funds?
^^^^^^^^^^^^^^^^

We still need to test that we can recover all tokens from the escrow,
and do so fairly. Recall our previous finishing strategy:

.. literalinclude:: Escrow4.hs
   :start-after: START oldFinishingStrategy
   :end-before: END oldFinishingStrategy

If we just use this as it is, it will fail. As before, we begin by
testing ``prop_FinishEscrow``, before we worry about unilateral
strategies for individual wallets:

  .. code-block:: text

   > quickCheck prop_FinishEscrow
   *** Failed! Falsified (after 5 tests and 5 shrinks):
   BadPrecondition
     [Do $ Init (Slot {getSlot = 3}) [],
      Do $ Pay (Wallet 3) 2]
     [Action (Refund (Wallet 3))]
     (EscrowModel {_contributions = fromList [(Wallet 3,Value (Map [(,Map [("",2000000)])]))],
                   _targets = fromList [],
                   _refundSlot = Slot {getSlot = 3},
                   _phase = Running})

In this test we set the deadline to slot 3, make a payment, and then
the finishing strategy attempts to refund the payment... in slot
two. It doesn't work: the precondition forbids a refund in that
slot. So we have to adapt our finishing strategy, which must simply
wait until the deadline before refunding the contributions.

.. literalinclude:: Escrow4.hs
   :start-after: START finishingStrategy
   :end-before: END finishingStrategy

To wait until the deadline, we use :hsobj:`Plutus.Contract.Test.ContractModel.Interface.waitUntilDL`; since this fails
if we try to wait until a slot in the past, then we have to check the
:hsobj:`Plutus.Contract.Test.ContractModel.Interface.currentSlot` (maintained by the model) before we decide whether or
not to wait.

.. literalinclude:: Escrow4.hs
   :start-after: START waitUntilDeadline
   :end-before: END waitUntilDeadline

With this extended strategy, the property passes:

  .. code-block:: text

   > quickCheck prop_FinishEscrow
   +++ OK, passed 100 tests.

   Actions (3588 in total):
   68.87% WaitUntil
   20.71% Pay
    4.77% Refund
    3.18% Redeem
    2.48% Init

   Refunded wallets (100 in total):
   67% 0
   13% 2
    7% 1
    6% 3
    6% 4
    1% 5


The strategy works, but the statistics we gathered on the number of
wallets to be refunded in each test are a little suspect. *In two
thirds of the tests, there were no refunds to be made!* This is not
ideal, given that we are testing whether or not our refund strategy
works.

This leads us to wonder: *which phase of the test did we reach* before
testing our finishing strategy? To find out, we can just add a couple
of lines to the ``finishingStrategy`` code, to :hsobj:`Plutus.Contract.Test.ContractModel.Interface.monitor` the phase:

.. literalinclude:: Escrow4.hs
   :start-after: START monitoredFinishingStrategy
   :end-before: END monitoredFinishingStrategy

Testing the property again, we see

  .. code-block:: text

   Phase (100 in total):
   68% Refunding
   32% Running

So in two thirds of our tests, we had already reached the
``Refunding`` phase before the finishing strategy was invoked--which
means, in many cases, that the addition we made to the strategy was
not needed.

While we certainly want to run *some* tests of the finishing strategy
starting in the ``Refunding`` phase, two thirds seems far too
many. How can we ensure that more tests invoke the strategy in the
``Running`` phase? The simplest way is just to *choose longer
deadlines*. There is no particular reason why QuickCheck's default
positive integer distribution should be the right one for
deadlines. The simplest way to increase the values chosen is just to
apply QuickCheck's |scale|_ combinator to the generator concerned:

.. |scale| replace:: ``scale``
.. _scale: https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html#v:scale

.. literalinclude:: Escrow4.hs
   :start-after: START weightedArbitraryAction
   :end-before: END weightedArbitraryAction

Here we scale the positive integer generator by multiplying the
QuickCheck size parameter by ten before generating; the effect is to
increase the range of values by a factor of ten.

Is ten the right number? The only way to tell is to run tests and
measure how often we reach the refunding stage:

  .. code-block:: text

   > quickCheck . withMaxSuccess 1000 $ prop_FinishFast
   +++ OK, passed 1000 tests.

   Phase (1000 in total):
   81.5% Running
   18.5% Refunding

   Refunded wallets (1000 in total):
   34.1% 0
   18.5% 1
   17.3% 2
   13.5% 3
   11.2% 4
    5.4% 5

It seems that we reach the refunding stage in around 20% of tests,
which seems reasonable. Moreover the propertion of cases in which
there are no refunds to be made is now lower--one third instead of two
thirds. So this is a useful improvement.

Finally, we also need to update the unilateral strategy for each
wallet in the same way. Once we have done so, then
``prop_NoLockedFunds`` passes again.

Digression: testing the model alone for speed
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We ran a thousand tests to measure the proportion that reach the
refunding stage, because one hundred tests is rather few to estimate
this percentage from. In fact even a thousand tests is rather few to
get accurate results; repeating that thousand-test run ten times
yielded a refunding-percentage ranging from 17.4% to 21.6%. Ideally
one might run millions of tests to measure the distribution, so we can tune
the generation more accurately. Yet running a thousand tests is already
quite slow, because of the speed of the emulator.

However, *it is not actually necessary to run the tests, to measure
their distribution*! The measurements we are making *depend only on
the model*, and so we can make them much more rapidly by taking the
emulator out of the test. This is simple to do: recall we defined
``prop_FinishEscrow`` by

.. literalinclude:: Escrow4.hs
   :start-after: START prop_FinishEscrow
   :end-before: END prop_FinishEscrow

which *generates* a test case from the dynamic logic test scenario
``finishEscrow``, and then *runs* it using ``prop_Escrow``. All we
have to do to take out the emulator is to replace ``prop_Escrow`` by
the property that is always ``True``:

.. literalinclude:: Escrow4.hs
   :start-after: START prop_FinishFast
   :end-before: END prop_FinishFast

This property generates tests in exactly the same way, and gathers
statistics in the same way (and checks preconditions in the same
way), but does not actually run the test on the emulator. In other
words, it's an excellent test of the *model*, and can be used to tune
it (or find bugs in it) without the cost of emulation.

With this version, we can at least run 100,000 tests in a short time,
and obtain much more accurate statistics:

  .. code-block:: text

   > quickCheck . withMaxSuccess 100000 $ prop_FinishFast
   +++ OK, passed 100000 tests.

   Phase (100000 in total):
   80.514% Running
   19.486% Refunding

   Refunded wallets (100000 in total):
   34.204% 0
   18.387% 1
   17.514% 2
   14.877% 3
   10.016% 4
    5.002% 5

The results confirm that the distribution of test cases is reasonably good.

Exercises
^^^^^^^^^

You will find the code discussed in this section in ``Spec.Tutorial.Escrow4``.

#. Run ``quickCheck prop_Escrow`` and observe the distributions
   reported. You will see that, even though we have extended the
   escrow deadlines, many actions are still rejected by their
   preconditions. Adapt the *generator* for actions, so that it only
   generates each action during the correct phase. How does that
   affect the proportion of rejected actions?

#. The supplied code still has a buggy ``walletStrategy``. Verify this
   by checking that ``prop_NoLockedFunds`` fails, and inspect the
   counterexample. Correct the ``walletStrategy``, and veryify that
   ``prop_NoLockedFunds`` now passes.

#. The code also contains a fast version of ``prop_NoLockedFunds``
   that does not run the emulator. Use *this* property to test your
   model, with and without the fix to the ``walletStrategy``. You
   should find that the bug is found anyway (it is at the model
   level), and that verifying that it has been fixed runs satisfyingly
   faster.

#. Modify the provided code to *remove* the scaling we applied to the
   deadline generator, and test ``prop_FinishFast`` repeatedly to
   judge the effect on the test case distribution. Reinsert the bug in
   ``walletStrategy``, and use

   .. code-block:: text

      quickCheck . withMaxSuccess 10000 $ prop_NoLockedFundsFast

   to find it. Run this repeatedly, and make an estimate of the number
   of tests needed to find the bug. Reinsert the scaling, and repeat
   your estimate. Hopefully this will help persuade you of the value
   of tuning your test case distributions!


Measuring coverage of on-chain code
-----------------------------------

It is always good practice to measure the source-code coverage of
tests. Coverage information provides a sanity check that nothing has
been missed altogether: while covering a line of code is no guarantee
that a bug in that line will be revealed, *failing to cover* a line of
code *does* guarantee that any bug there will *not* be found. For
critical code, it is reasonable to aim for 100% coverage.

Coverage of Haskell code can be measured using the `Haskell Program
Coverage <https://wiki.haskell.org/Haskell_program_coverage>`_
toolkit; we will not discuss this further here. But while this works
well for measuring the coverage of *off-chain* code, it does not apply
to *on-chain* code, because this is compiled using the Plutus compiler
and executed on the blockchain, rather than by GHC. If we want to
measure the coverage of *on-chain* code--which is the most critical
code in a Plutus contract--then we need to use a separate tool. This
is what we cover in this section.

Adding a coverage index
^^^^^^^^^^^^^^^^^^^^^^^

In order to generate a coverage report, the framework needs to know

#. what was covered by tests,

#. what should have been covered.

Indeed, the most important part of a coverage report is often the
parts that were *not* covered by tests. This latter information--what
should be covered--is represented by a :hsobj:`PlutusTx.Coverage.CoverageIndex` that the
Plutus compiler constructs. Since the Plutus compiler is invoked using
Template Haskell in the code of the contract itself, then this is
where we have to save, and export, the coverage index. That is, we
must make additions to the code of a contract in order to enable
coverage measurement.

To do so, we first inspect the code, and find all the
occurrences of ``PlutusTx.compile``. In the case of the escrow
contract, they are in the definition of :hsobj:`Plutus.Contracts.Escrow.typedValidator`:

 .. literalinclude:: EscrowImpl.hs
   :start-after: START typedValidator
   :end-before: END typedValidator

The on-chain code consists of ``validate`` and ``wrap``. The latter is
a library function, whose coverage we do not need to measure, so we
just add (and export) a definition of a :hsobj:`PlutusTx.Coverage.CoverageIndex` that covers
``validate``:

  .. literalinclude:: EscrowImpl.hs
   :start-after: START covIdx
   :end-before: END covIdx

.. note::

   It is important that the coverage index is computed in the same module as
   the calls to ``PlutusTx.compile``, or else the Haskell compiler--and thus by
   extension, the Plutus compiler--may produce different code for the coverage
   index and for the code under test, resulting in misleading coverage reports.

It just remains to *import* the necessary types and functions

 .. literalinclude:: EscrowImpl.hs
   :start-after: START imports
   :end-before: END imports

and to supply GHC options that cause the Plutus compiler to generate
coverage information:

.. literalinclude:: EscrowImpl.hs
   :start-after: START OPTIONS_GHC
   :end-before: END OPTIONS_GHC

With these additions, the contract implementation is ready for
coverage measurement.

Measuring coverage
^^^^^^^^^^^^^^^^^^

Once we have created a suitable :hsobj:`PlutusTx.Coverage.CoverageIndex`, we must create a
test that uses it. To do so, we need to

#. Run the test using :hsobj:`Plutus.Contract.Test.ContractModel.Interface.quickCheckWithCoverage`, and give it coverage options specifying the coverage index,

#. Pass the (modified) coverage options that :hsobj:`Plutus.Contract.Test.ContractModel.Interface.quickCheckWithCoverage` constructs in to :hsobj:`Plutus.Contract.Test.ContractModel.Interface.propRunActionsWithOptions` (instead of :hsobj:`Plutus.Contract.Test.ContractModel.Interface.propRunActions_`) when we run the action sequence, and

#. (Ideally) visualize the resulting :hsobj:`PlutusTx.Coverage.CoverageReport` as annotated source code.

Here is the code to do all this (we also need to import :hsmod:`Plutus.Contract.Test.Coverage`):

 .. literalinclude:: Escrow5.hs
   :start-after: START check_propEscrowWithCoverage
   :end-before: END check_propEscrowWithCoverage

First we call :hsobj:`Plutus.Contract.Test.ContractModel.Interface.quickCheckWithCoverage` with options containing
``covIdx``; it passes modified options to the rest of the property. We
test the property 1000 times, so that we are very likely to cover all
the reachable code in the tests. We cannot just reuse ``prop_Escrow``,
because we must pass in the modified coverage options ``covopts`` when
we run the actions, but otherwise this is just the same as
``prop_Escrow``. The result returned by :hsobj:`Plutus.Contract.Test.ContractModel.Interface.quickCheckWithCoverage` is
a :hsobj:`PlutusTx.Coverage.CoverageReport`, which is difficult to interpret by itself, so we
bind it to ``cr`` and then generate an HTML file ``Escrow.html`` using
:hsobj:`Plutus.Contract.Test.Coverage.writeCoverageReport`.

Running this does take a little while, because we run a large number of
tests; on the other hand, diagnosing *why* a part of the code has not
been covered can be very time-consuming, and is wasted effort if the
reason is simply that we were unlucky when we ran the tests. It is
worth waiting a few minutes for more accurate coverage data, before
starting this kind of diagnosis.

Quite a lot of output is generated, including lists of coverage items
that were covered respectively not covered. We shall ignore these for
now; the same information is presented much more readably in the
generated HTML file. But note that we do see statistics on endpoint
invocations:

  .. code-block:: text

   > check_propEscrowWithCoverage
   +++ OK, passed 1000 tests:
   63.1% Contract instance for W[4] at endpoint pay-escrow
   62.5% Contract instance for W[1] at endpoint pay-escrow
   62.5% Contract instance for W[2] at endpoint pay-escrow
   61.2% Contract instance for W[3] at endpoint pay-escrow
   60.8% Contract instance for W[5] at endpoint pay-escrow
   29.1% Contract instance for W[5] at endpoint redeem-escrow
   28.2% Contract instance for W[1] at endpoint redeem-escrow
   27.4% Contract instance for W[3] at endpoint redeem-escrow
   25.8% Contract instance for W[2] at endpoint redeem-escrow
   25.6% Contract instance for W[4] at endpoint redeem-escrow
    4.5% Contract instance for W[1] at endpoint refund-escrow
    4.1% Contract instance for W[2] at endpoint refund-escrow
    3.9% Contract instance for W[4] at endpoint refund-escrow
    3.5% Contract instance for W[3] at endpoint refund-escrow
    3.3% Contract instance for W[5] at endpoint refund-escrow

   ...

This table tells us what percentage of test cases made a call to each
endpoint from the given wallet; for example, 63.1% of test cases made
(somewhere) a call to the ``pay-escrow`` endpoint from wallet 4. As we
can see, the ``pay-escrow`` endpoint is called in most tests from each
wallet, ``redeem-escrow`` is a bit rarer, and ``refund-escrow`` is
used quite rarely. Most serious, of course, would be if one of the
endpoints doesn't appear in this table at all.

It is possible to supply coverage goals for each wallet/endpoint
combination via an additional coverage option. We don't consider this
further here, except to note that by default the framework expects
each combination to appear in 20% of tests, and so we get warnings in
this case:

  .. code-block:: text

      Only 4.5% Contract instance for W[1] at endpoint refund-escrow, but expected 20.0%
      Only 4.1% Contract instance for W[2] at endpoint refund-escrow, but expected 20.0%
      Only 3.5% Contract instance for W[3] at endpoint refund-escrow, but expected 20.0%
      Only 3.9% Contract instance for W[4] at endpoint refund-escrow, but expected 20.0%
      Only 3.3% Contract instance for W[5] at endpoint refund-escrow, but expected 20.0%

These warnings can be eliminated by specifying more appropriate (lower)
coverage goals for these endpoint calls.

Interpreting the coverage annotations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Running the test above writes annotated source code to
``Escrow.html``. The entire contents of the file are reproduced
:ref:`here<CoverageReport>`. The report contains all of the
on-chain code provided in the :hsobj:`PlutusTx.Coverage.CoverageIndex`, together with a few
lines of code around it for context. Off-chain code appears in grey,
so it can be distinguished. On-chain code on a white background was
covered by tests, and we know no more about it. Code on a red or green
background was also covered, but it is a boolean expression, and only
took one value (red for ``False``, green for ``True``). Orange code on
a black background is on-chain code that was not covered at all--and
thus may represent a gap in testing.

Looking at the last section of code in the report,

  .. raw:: html

   <pre>
      365    <span style=background-color:lightgray;color:gray >covIdx :: CoverageIndex</span>
      366    <span style=background-color:lightgray;color:gray >covIdx = getCovIdx $$(</span><span style=background-color:black;color:orangered >PlutusTx.compile [|| val
   </pre>

we see that it is the construction of the coverage index, and
parts of this code are labelled on-chain and uncovered. We can ignore
this, it's simply an artefact of the way the code labelling is done.

More interesting is the second section of the report:

  .. raw:: html

   <pre>
      201    <span style=background-color:lightgray;color:gray >{-# INLINABLE validate #-}</span>
      202    <span style=background-color:lightgray;color:gray >validate :: EscrowParams DatumHash -&gt; PaymentPubKeyHash -&gt; Action -&gt; ScriptContext -&gt; Bool</span>
      203    <span style=background-color:white;color:black >validate EscrowParams{escrowDeadline, escrowTargets} contributor action ScriptContext{scriptContextTxInfo} =</span><span style=background-color:lightgray;color:gray ></span>
      204    <span style=background-color:white;color:black >    case action of</span><span style=background-color:lightgray;color:gray ></span>
      205    <span style=background-color:white;color:black >        Redeem -&gt;</span><span style=background-color:lightgray;color:gray ></span>
      206    <span style=background-color:white;color:black >            traceIfFalse </span><span style=background-color:black;color:orangered >&quot;escrowDeadline-after&quot; </span><span style=background-color:white;color:black >(escrowDeadline `after` txInfoValidRange scriptContextTxInfo)</span><span style=background-color:lightgray;color:gray ></span>
      207    <span style=background-color:white;color:black >            &amp;&amp; </span><span style=background-color:lightgreen;color:black >traceIfFalse </span><span style=background-color:black;color:orangered >&quot;meetsTarget&quot; </span><span style=background-color:lightgreen;color:black >(all (meetsTarget scriptContextTxInfo) escrowTargets)</span><span style=background-color:white;color:black ></span><span style=background-color:lightgray;color:gray ></span>
      208    <span style=background-color:white;color:black >        Refund -&gt;</span><span style=background-color:lightgray;color:gray ></span>
      209    <span style=background-color:white;color:black >            </span><span style=background-color:lightgreen;color:black >traceIfFalse </span><span style=background-color:black;color:orangered >&quot;escrowDeadline-before&quot; </span><span style=background-color:lightgreen;color:black >((escrowDeadline - 1) `before` txInfoValidRange scriptContextTxInfo)</span><span style=background-color:lightgray;color:gray ></span>
      210    <span style=background-color:lightgreen;color:black >            &amp;&amp; </span><span style=background-color:lightgreen;color:black >traceIfFalse </span><span style=background-color:black;color:orangered >&quot;txSignedBy&quot; </span><span style=background-color:lightgreen;color:black >(scriptContextTxInfo `txSignedBy` unPaymentPubKeyHash contributor)</span><span style=background-color:lightgray;color:gray ></span>
      211    <span style=background-color:lightgray;color:gray ></span>
   </pre>

This is the main validator, and while some of its code is coloured
white, much of it is coloured green. This means the checks in this
function always returned ``True`` in our tests; we have not tested the
cases in which they return ``False``.

This does indicate a weakness in our testing: since these checks
always passed in our tests, then those tests would *also* have passed
if the checks were removed completely (replaced by ``True``)--but the
contract would have been quite wrong. We will return to this point
later, when we discuss *negative testing*. For now, though, we just
note that *if the checks had returned* ``False``, *then the
transaction would have failed*--and the off-chain code is, of course,
designed not to submit failing transactions. So, in a sense, we should
expect this code to be coloured green--at least, when we test through
well-designed off-chain endpoints, as we have been doing.

This code fragment also contains some entirely uncovered code--the
strings passed to :hsobj:`PlutusTx.Trace.traceIfFalse` to be used as error messages if a
check fails. Since correct off-chain code never submits failing
transactions, then these error messages are never used--and hence the
code is labelled as 'uncovered'. Again, this is not really a problem.

The most interesting part of the report is the first section:

  .. raw:: html

   <pre>
      190    <span style=background-color:lightgray;color:gray >meetsTarget :: TxInfo -&gt; EscrowTarget DatumHash -&gt; Bool</span>
      191    <span style=background-color:white;color:black >meetsTarget ptx = \case</span><span style=background-color:lightgray;color:gray ></span>
      192    <span style=background-color:white;color:black >    PaymentPubKeyTarget pkh vl -&gt;</span><span style=background-color:lightgray;color:gray ></span>
      193    <span style=background-color:white;color:black >        </span><span style=background-color:lightgreen;color:black >valuePaidTo ptx (unPaymentPubKeyHash pkh) `geq` vl</span><span style=background-color:white;color:black ></span><span style=background-color:lightgray;color:gray ></span>
      194    <span style=background-color:white;color:black >    ScriptTarget validatorHash dataValue vl -&gt;</span><span style=background-color:lightgray;color:gray ></span>
      195    <span style=background-color:white;color:black >        </span><span style=background-color:black;color:orangered >case scriptOutputsAt </span><span style=background-color:black;color:orangered >validatorHash </span><span style=background-color:black;color:orangered >ptx </span><span style=background-color:black;color:orangered >of</span><span style=background-color:lightgray;color:gray ></span>
      196    <span style=background-color:black;color:orangered >            [(dataValue&#39;, vl&#39;)] -&gt;</span><span style=background-color:lightgray;color:gray ></span>
      197    <span style=background-color:black;color:orangered >                </span><span style=background-color:black;color:orangered >traceIfFalse </span><span style=background-color:black;color:orangered >&quot;dataValue&quot; </span><span style=background-color:black;color:orangered >(</span><span style=background-color:black;color:orangered >dataValue&#39; </span><span style=background-color:black;color:orangered >== </span><span style=background-color:black;color:orangered >dataValue)</span><span style=background-color:black;color:orangered ></span><span style=background-color:lightgray;color:gray ></span>
      198    <span style=background-color:black;color:orangered >                &amp;&amp; </span><span style=background-color:black;color:orangered >traceIfFalse </span><span style=background-color:black;color:orangered >&quot;value&quot; </span><span style=background-color:black;color:orangered >(</span><span style=background-color:black;color:orangered >vl&#39; </span><span style=background-color:black;color:orangered >`geq` </span><span style=background-color:black;color:orangered >vl)</span><span style=background-color:black;color:orangered ></span><span style=background-color:black;color:orangered ></span><span style=background-color:lightgray;color:gray ></span>
      199    <span style=background-color:black;color:orangered >            _ -&gt; </span><span style=background-color:black;color:orangered >False</span><span style=background-color:lightgray;color:gray ></span>
   </pre>

This is the function that is used to check that each target payment is
made when the escrow is redeemed, and as we see from the coverage
report, there are two cases, of which only one has been tested. In
fact the two cases handle payments to a wallet, and payments to a
script, and the second kind of payment is *not tested at all* by our
tests--yet it is handled by entirely different code in the on-chain
function.

**This exposes a serious deficiency in the tests developed so far**:
they give us no evidence at all that target payments to a script work
as intended. To test this code as well, we would need to add 'proxy'
contracts to the tests, to act as recipients for such payments. We
leave making this extension as an exercise for the reader.

.. _CoverageReport:

The generated coverage report
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This is the generated coverage report in its entirety:

   .. raw:: html
      :file: Escrow.html

Crashes, and how to tolerate them
---------------------------------

One awkward possibility, that we cannot avoid, is that a contract
instance might crash during execution--for example, because of a power
failure to the machine it is running on. We don't want anything to be
lost permanently as a result--it should be possible to recover by
restarting the contract instance, perhaps in a different state, and
continue. Yet how should we test this? We need to deliberately crash
and restart contracts in tests, and check that they still behave as
the model says they should.

The :hsobj:`Plutus.Contract.Test.ContractModel.Interface.ContractModel` framework provides a simple way to *extend* a
contract model, so that it can test crash-tolerance too. If ``m`` is a
:hsobj:`Plutus.Contract.Test.ContractModel.Interface.ContractModel` instance, then so is ``WithCrashTolerance m``--and
testing the latter model will insert actions to crash and restart
contract instances at random. To define a property that runs these
tests, all we have to do is import :hsmod:`Plutus.Contract.Test.ContractModel.CrashTolerance` and include
:hsobj:`Plutus.Contract.Test.ContractModel.CrashTolerance.WithCrashTolerance` in the type
signature:

 .. literalinclude:: Escrow6.hs
   :start-after: START prop_CrashTolerance
   :end-before: END prop_CrashTolerance

(The actual code here is the same as ``prop_Escrow``, only the type is different).

We do have to provide a little more information before we can run
tests, though.

#. Firstly, we cannot expect to include an action in a
   test, when the contract(s) that should perform the action are not
   running. We thus need to tell the framework whether or not an action
   is *available*, given the contracts currently running.

#. Secondly, when we restart a contract it may need to take some
   recovery action, and so we must be able to give it the necessary
   information to recover. We achieve this by specifying
   possibly-different contract parameters to use, when a contract is
   restarted. These parameters may depend on the model state.

We provide this information by defining an instance of the
:hsobj:`Plutus.Contract.Test.ContractModel.CrashTolerance.CrashTolerance` class:

 .. literalinclude:: Escrow6.hs
   :start-after: START CrashTolerance
   :end-before: END CrashTolerance

The :hsobj:`Plutus.Contract.Test.ContractModel.CrashTolerance.available` method returns ``True`` if an action is available,
given a list of active contract keys ``alive``; since contract
instance keys have varying types, then the list actually contains keys
wrapped in an existential type, which is why the ``Key`` constructor
appears there.

The :hsobj:`Plutus.Contract.Test.ContractModel.CrashTolerance.restartArguments` method provides the parameter for restarting
an escrow contract, which in this case can be just the same as when
the contract was first started. We need to recover the targets from
the model state, in which they are represented as a ``Map Wallet
Value``, so we convert them back to a list and refactor the
``escrowParams`` function so we can give ``escrowParams'`` a list of
``(Wallet, Value)`` pairs, rather than a list of ``(Wallet, Int)``:

 .. literalinclude:: Escrow6.hs
   :start-after: START escrowParams
   :end-before: END escrowParams

It is possible to define the effect of crashing or restarting a
contract instance on the *model* too, if need be, by defining
additional methods in this class. In this case, though, crashing and
restarting ought to be entirely transparent, so we can omit them.

Surprisingly, the tests do not pass!

  .. code-block:: text

   > quickCheck prop_CrashTolerance
   *** Failed! Assertion failed (after 24 tests and 26 shrinks):
   Actions
    [Init (Slot {getSlot = 6}) [(Wallet 1,2),(Wallet 4,2)],
     Crash (WalletKey (Wallet 4)),
     Restart (WalletKey (Wallet 4)),
     Pay (Wallet 4) 4,
     Redeem (Wallet 1)]
   Expected funds of W[4] to change by
     Value (Map [(,Map [("",-2000000)])])
   but they changed by
     Value (Map [(,Map [("",-4000000)])])
   a discrepancy of
     Value (Map [(,Map [("",-2000000)])])
   Expected funds of W[1] to change by
     Value (Map [(,Map [("",2000000)])])
   but they did not change
   Contract instance log failed to validate:
   ...
   Slot 5: 00000000-0000-4000-8000-000000000000 {Wallet W[1]}:
             Contract instance stopped with error: RedeemFailed NotEnoughFundsAtAddress
   ...

Here we simply set up targets with two beneficiaries, crash and
restart wallet 4, pay sufficient contributions to cover the targets,
and then try to redeem the escrow, which seems straightforward enough,
and yet the redemption thinks there are not enough funds in the
escrow, *even though we just paid them in*!

This failure is a little tricky to debug. A clue is that the *payment*
was made by a contract instance that has been restarted, while the
*redemption* was made by a contract that has not. Do the payment and
redemption actually refer to the same escrow? In fact the targets
supplied to the contract instance are not necessarily exactly the
same: the contract receives a *list* of targets, but in the model we
represented them as a *map*--and converted the list of targets to a
map, and back again, when we restarted the contract. That means the
*order* of the targets might be different.

Could that make a difference? To find out, we can just *sort* the
targets before passing them to the contract instance, thus
guaranteeing the same order every time:

 .. literalinclude:: Escrow6.hs
   :start-after: START betterEscrowParams
   :end-before: END betterEscrowParams

Once we do this, the tests pass. We can also see from the resulting
statistics that quite a lot of crashing and restarting is going on:

  .. code-block:: text

   > quickCheck prop_CrashTolerance
   +++ OK, passed 100 tests.

   Actions (2721 in total):
   42.48% Pay
   24.99% WaitUntil
   13.08% Crash
    9.52% Restart
    6.06% Redeem
    3.01% Init
    0.85% Refund

Perhaps it's debatable whether or not the behaviour we uncovered here
is a *bug*, but it is certainly a feature--it was not obvious in
advance that specifying the same targets in a different order would
create an independent escrow, but that is what happens. So for
example, if a buyer and seller using an escrow contract to exchange an
NFT for Ada specify the two targets in different orders, then they
would place their assets in independent escrow that cannot be redeemed
until the refund deadline passes. Arguably a better designed contract
would sort the targets by wallet, as we have done here, before
creating any UTXOs, so that the problem cannot arise.

Exercises
^^^^^^^^^

You will find the code discussed here in ``Spec.Tutorial.Escrow6``, *without* the addition of ``sortBy`` to ``escrowParams``.

#. Run ``quickCheck prop_CrashTolerance`` to provoke a test
   failure. Examine the counterexample and the test output, and make
   sure you understand how the test fails. Run this test several
   times: you will see the failure in several different forms, with
   the same underlying cause. Make sure you understand how each
   failure arises.

   Why does |quickCheck|_ always report a test case with *two* target
   payments--why isn't one target enough to reveal the problem?

#. Add sorting to the model, and verify that the tests now pass.

#. An alternative way to fix the model is *not* to convert the targets
   to a ``Map`` in the model state, but just keep them as a list of
   pairs, so that exactly the same list can be supplied to the
   contract instances when they are restarted. Implement this change,
   and verify that the tests still pass.

   Which solution do you prefer? Arguably this one reflects the
   *actual* design of the contract more closely, since the model makes
   explicit that the order of the targets matters.


Debugging the Auction contract with model assertions
----------------------------------------------------

In this section, we'll apply the techniques we have seen so far to
test another contract, and we'll see how they reveal some surprising
behaviour.  The contract we take this time is the auction contract in
:hsmod:`Plutus.Contracts.Auction`. This module actually defines *two*
contracts, a seller contract and a buyer contract. The seller puts up
a :hsobj:`Ledger.Value` for sale, creating an auction UTXO containing the value,
and buyers can then bid Ada for it. When the auction deadline is
reached, the highest bidder receives the auctioned value, and the
seller receives the bid.

Modelling the Auction contract
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``Spec.Auction`` contains a contract model for testing this
contract. The value for sale is a custom token, wallet 1 is the
seller, and the deadline used for testing is fixed at slot 101; the
generated tests just consist of an ``Init`` action to start the
auction, and a series of ``Bid`` actions by the other wallets.

 .. literalinclude:: Auction.hs
   :start-after: START Action
   :end-before: END Action

The model keeps track of the highest bid and bidder, and the current
phase the auction is in:

 .. literalinclude:: Auction.hs
   :start-after: START model
   :end-before: END model

It is updated by the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.nextState` method on each bid:

 .. literalinclude:: Auction.hs
   :start-after: START nextState
   :end-before: END nextState

Note that when a higher bid is received, the previous bid is returned
to the bidder.

We only allow bids that are larger than the previous one (which is why
:hsobj:`Plutus.Contract.Test.ContractModel.Interface.nextState` doesn't need to check this):

 .. literalinclude:: Auction.hs
   :start-after: START precondition
   :end-before: END precondition

The most interesting part of the model covers what happens when the
auction deadline is reached: in contrast to the ``Escrow`` contract,
the highest bid is paid to the seller automatically, and the buyer
receives the token. We model this using the :hsobj:`Plutus.Contract.Test.ContractModel.Interface.nextReactiveState`
method introduced in section :ref:`Timing`

 .. literalinclude:: Auction.hs
   :start-after: START nextReactiveState
   :end-before: END nextReactiveState

Finally we can define the property to test; in this case we have to
supply some options to initialize wallet 1 with the token to be
auctioned:

 .. literalinclude:: Auction.hs
   :start-after: START prop_Auction
   :end-before: END prop_Auction

The only important part here is ``options``, which is defined as follows:

 .. literalinclude:: Auction.hs
   :start-after: START options
   :end-before: END options

Unsurprisingly, the tests pass.

  .. code-block:: text

   > quickCheck prop_Auction
   +++ OK, passed 100 tests.

   Actions (2348 in total):
   85.82% Bid
   10.35% WaitUntil
    3.83% Init

No locked funds?
^^^^^^^^^^^^^^^^

Now we have a basic working model of the auction contract, we can
begin to test more subtle properties. To begin with, can we recover
the funds held by the contract? The strategy to try is obvious: all we
have to do is wait for the deadline to pass. So ``prop_FinishAuction``
is very simple:

 .. literalinclude:: Auction.hs
   :start-after: START prop_FinishAuction
   :end-before: END prop_FinishAuction

This property passes too:

  .. code-block:: text

   > quickCheck prop_FinishAuction
   +++ OK, passed 100 tests.

   Actions (3152 in total):
   84.77% Bid
   12.25% WaitUntil
    2.98% Init

Now, to supply a :hsobj:`Plutus.Contract.Test.ContractModel.Interface.NoLockedFundsProof` we need a general strategy for
fund recovery, and a wallet-specific one. Since all we have to do is
wait, we can use the *same* strategy as both.

 .. literalinclude:: Auction.hs
   :start-after: START noLockProof
   :end-before: END noLockProof

Surprisingly, *these tests fail*!

  .. code-block:: text

   > quickCheck prop_NoLockedFunds
   *** Failed! Assertion failed (after 2 tests and 1 shrink):
   DLScript
     [Do $ Init,
      Do $ Bid (Wallet 3) 2000000]

   The ContractModel's Unilateral behaviour for Wallet 3 does not match the
   actual behaviour for actions:
   Actions
    [Var 0 := Init,
     Var 1 := Bid (Wallet 3) 2000000,
     Var 2 := Unilateral (Wallet 3),
     Var 3 := WaitUntil (Slot {getSlot = 101})]
   Expected funds of W[1] to change by
     Value (Map [(363d...,Map [("token",-1)])])
   but they changed by
     Value (Map [(,Map [("",-2000000)]),(363d...,Map [("token",-1)])])
   a discrepancy of
     Value (Map [(,Map [("",-2000000)])])
   Expected funds of W[3] to change by
     Value (Map [(363d...,Map [("token",1)])])
   but they changed by
     Value (Map [(,Map [("",-2000000)])])
   a discrepancy of
     Value (Map [(,Map [("",-2000000)]),(363d...,Map [("token",-1)])])
   Test failed.

This test just started the auction and submitted a bid from wallet 3,
then *stopped all the other wallets* (this is what ``Unilateral
(Wallet 3)`` does), before waiting until the auction deadline.  This
resulted in a different distribution of funds from the one the model
predicts.  Looking at the last part of the message, we see that we
expected wallet 3 to get the token, *but it did not*; neither did it
get its bid back. Wallet 1 did lose the token, though, and in addition
lost the 2 Ada required to create the auction UTXO in the first place.

What is going on? The strategy worked in the general case, but failed
in the unilateral case, which tells us that *the buyer requires the
cooperation of the seller* in order to recover the auctioned
token. Why? Well, our description of the contract above was a little
misleading: the proceeds of the auction *cannot* be paid out
automatically just because the deadline passes; the Cardano blockchain
won't do that. Instead, *someone must submit the payout
transaction*. In the case of this contract, it's the seller: even
though there is no *endpoint call* at the deadline, the seller's
off-chain code continues running throughout the auction, and when the
deadline comes it submits the payout transaction. So if the seller's
contract is stopped, then no payout occurs.

This is not a *very* serious bug, because the *on-chain* code allows
anyone to submit the payout transaction, so the buyer could in
principle do so. However, the existing off-chain code does not provide
an endpoint for this, and so recovering the locked funds would require
writing a new version of the off-chain code (or rolling a suitable
transaction by hand).

 .. _AuctionAssertion:

Model assertions, and unexpected expectations.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Looking back at the failed test again, the *expected* wallet contents
are actually a little *unexpected*:

  .. code-block:: text

   Actions
    [Var 0 := Init,
     Var 1 := Bid (Wallet 3) 2000000,
     Var 2 := Unilateral (Wallet 3),
     Var 3 := WaitUntil (Slot {getSlot = 101})]
   Expected funds of W[1] to change by
     Value (Map [(363d...,Map [("token",-1)])])
   but they changed by
     Value (Map [(,Map [("",-2000000)]),(363d...,Map [("token",-1)])])
   a discrepancy of
     Value (Map [(,Map [("",-2000000)])])

Notice that, even though wallet 3 made a bid of 2 Ada, we *expected*
the seller to end up without the token, but *with no extra
money*. Wouldn't we expect the seller to end up with 2 Ada?

Because ``prop_Auction`` passed, then we know that in the absence of a
``Unilateral`` then the model and the contract implementation agree on
fund transfers. But does the model actually predict that the seller
gets the winning bid? This can be a little hard to infer from the
state transitions themselves; we can check that each action appears to
do the right thing, but whether the end result is as expected is not
necessarily immediately obvious.

We can address this kind of problem by *adding assertions to the
model*. The model tracks the change in each wallet's balance since the
beginning of the test, so we can add an assertion, at the point where
the auction ends, that checks that the seller loses the token and
gains the winning bid. We just a little code to :hsobj:`Plutus.Contract.Test.ContractModel.Interface.nextReactiveState`:

 .. literalinclude:: Auction.hs
   :start-after: START extendedNextReactiveState
   :end-before: END extendedNextReactiveState

If the boolean passed to :hsobj:`Plutus.Contract.Test.ContractModel.Interface.assertSpec` is ``False``, then the test
fails with the first argument in the error message.

 .. note::

    We do have to allow for the possibility that the auction never
    started, which is why we include in the assertion the possibility
    that wallet 1's balance remains unchanged. Without this, the tests
    fail.

Now ``prop_Auction`` fails!

  .. code-block:: text

   > quickCheck prop_Auction
   *** Failed! Falsified (after 27 tests and 24 shrinks):
   Actions
    [Init,
     Bid (Wallet 3) 2000000,
     WaitUntil (Slot {getSlot = 100})]
   assertSpec failed: w1 final balance is wrong:
     SymValue {symValMap = fromList [], actualValPart = Value (Map [(363d...,Map [("token",-1)])])}

 .. note::

    The balance change is actually a :hsobj:`Plutus.Contract.Test.ContractModel.Symbolics.SymValue`, not a :hsobj:`Ledger.Value`,
    but as you can see it *contains* a :hsobj:`Ledger.Value`, which is all we care
    about right now.

Even in this simple case, the seller does not receive the right
amount: wallet 1 lost the token, but received no payment!

The reason has to do with the minimum Ada in each UTXO. When the
auction UTXO is created, the seller has to put in 2 Ada along with the
token. When the auction ends, one might expect that 2 Ada to be
returned to the seller. But it can't be: *it is needed to create the
UTXO that delivers the token to the buyer*! Thus the seller receives 2
Ada (from the buyer's bid) in this example, but this only makes up for
the 2 Ada deposited in the auction UTXO, and the seller ends up giving
away the token for nothing.

This is quite surprising behaviour, and arguably, the contract should
require that the buyer pay the seller 2 Ada *plus* the winning bid, so that
the stated bid is equal to the seller's net receipts.

  .. note::

     Model assertions can be tested without running the emulator, by
     using :hsobj:`Plutus.Contract.Test.ContractModel.Interface.propSanityCheckAssertions` instead of
     :hsobj:`Plutus.Contract.Test.ContractModel.Interface.propRunActions_`. This is very much faster, and enables very
     thorough testing of the model. Since other tests check that the
     implementation correponds to the model, then this still gives us
     valuable information about the implementation.

Crashing the auction
^^^^^^^^^^^^^^^^^^^^

Is the auction crash tolerant? To find out, we just declare that
``Bid`` actions are available when the corresponding buyer contract is
running, define the restart arguments, and the crash-tolerant property
(which just replicates the definition of ``prop_Auction`` with a
different type).

 .. literalinclude:: Auction.hs
   :start-after: START crashTolerance
   :end-before: END crashTolerance

Perhaps unsurprisingly, this property fails:

  .. code-block:: text

   > quickCheck prop_CrashTolerance
   *** Failed! Assertion failed (after 17 tests and 11 shrinks):
   Actions
    [Init,
     Crash SellerH,
     WaitUntil (Slot {getSlot = 100})]
   Expected funds of W[1] to change by
     Value (Map [])
   but they changed by
     Value (Map [(,Map [("",-2000000)]),(363d3944282b3d16b239235a112c0f6e2f1195de5067f61c0dfc0f5f,Map [("token",-1)])])
   a discrepancy of
     Value (Map [(,Map [("",-2000000)]),(363d3944282b3d16b239235a112c0f6e2f1195de5067f61c0dfc0f5f,Map [("token",-1)])])
   Test failed.

We already know that the auction payout is initiated by the seller
contract, so if that contract is not running, then no payout takes
place. (Although there are no bids in this counterexample, a payout is
still needed--to return the token to the seller). That is why this test fails.

But this is actually not the only way the property can fail. The other
failure (which generates some rather long contract logs) looks like
this:

  .. code-block:: text

   > quickCheck prop_CrashTolerance
   *** Failed! Assertion failed (after 13 tests and 9 shrinks):
   Actions
    [Init,
     Crash SellerH,
     Restart SellerH]
   Contract instance log failed to validate:
   ... half a megabyte of output ...
   Slot 6: 00000000-0000-4000-8000-000000000004 {Wallet W[1]}:
             Contract instance stopped with error: StateMachineContractError (SMCContractError (WalletContractError (InsufficientFunds "Total: Value (Map [(,Map [(\"\",9999999997645750)])]) expected: Value (Map [(363d3944282b3d16b239235a112c0f6e2f1195de5067f61c0dfc0f5f,Map [(\"token\",1)])])")))
   Test failed.

In other words, after a crash, *the seller contract fails to
restart*. This is simply because the seller tries to put the token up
for auction when it starts, and *wallet 1 no longer holds the
token*--it is already in an auction UTXO on the blockchain. So the
seller contract fails with an :hsobj:`Wallet.Emulator.Error.InsufficientFunds` error. To continue
the auction, we would really need another endpoint to resume the
seller, which the contract does not provide, or a parameter to the
seller contract which specifies whether to start or continue an
auction.

Coverage of the Auction contract
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can generate a coverage report for the ``Auction`` contract just as
we did for the ``Escrow`` one. The interesting part of the report is:

  .. raw:: html
     :file: Auction.html

The auction is defined as a Plutus state machine, which just repeats
an ``auctionTransition`` over and over again. We can see that the
state machine itself, and most of the transition code, is
covered. However, the ``Bid`` transition has only been tested in the
case where new bid is higher than the old one. Indeed, the tests are
designed to respect that precondition. Moroever, the last clause in
the ``case`` expression has not been tested at all--but this is quite
OK, because it returns ``Nothing`` which the state machine library
interprets to mean "reject the transaction". So the uncovered code
*could* only be covered by failing transactions, which the off-chain
code is designed not to submit.

Exercises
^^^^^^^^^

The code discussed here is in ``Spec.Auction``.

#. Test the failing properties (``prop_NoLockedFunds`` and
   ``prop_CrashTolerance``) and observe the failures.

#. Add the model assertion discussed in :ref:`AuctionAssertion` to the
   code, and ``quickCheck prop_SanityCheckAssertions`` to verify that
   it fails. Change the assertion to say that the seller receives 2
   Ada *less* than the bid, and verify that it now passes.

Becoming Level 1 Certification Ready
------------------------------------

Level 1 certification of plutus smart contracts relies on the machinery
we have discussed in this tutorial. First things first we are going to
have a look at the :hsmod:`Plutus.Contract.Test.Certification` module.

This module defines a type :hsobj:`Plutus.Contract.Test.Certification.Certification` paramtereized over a type
``m`` that should be a :hsobj:`Plutus.Contract.Test.ContractModel.Interface.ContractModel`. This is a record type that has
fields for:

#. a :hsobj:`PlutusTx.Coverage.CoverageIndex`,
#. two different types of :hsobj:`Plutus.Contract.Test.ContractModel.Interface.NoLockedFundsProof`
   (a standard full proof and a light proof that does not require you to provide
   a per-wallet unilateral strategy),
#. the ability to provide a specialized error whitelist,
#. a way to specify that we have an instance of :hsobj:`Plutus.Contract.Test.ContractModel.CrashTolerance.CrashTolerance` for ``m``,
#. unit tests in the form of a function from a :hsobj:`Plutus.Contract.Test.Coverage.CoverageRef` to a |TestTree|_
   (see :hsobj:`Plutus.Contract.Test.checkPredicateCoverage`
   for how to construct one of these), and
#. named dynamic logic unit tests.

.. |TestTree| replace:: ``TestTree``
.. _TestTree: https://hackage.haskell.org/package/tasty-1.4.2.3/docs/Test-Tasty.html#t:TestTree

Fortunately, understanding what we need to do to get certification-ready
at this stage is simple. We just need to build a :hsobj:`Plutus.Contract.Test.Certification.Certification` object.
For example of how to do this, check out ``Spec.GameStateMachine.certification``
and ``Spec.Uniswap.certification``.

You can run level 1 certification locally using the
:hsobj:`Plutus.Contract.Test.Certification.Run.certify` function - but at
this stage you may find it difficult to read the output of this function.
Don't worry! A certification dashboard is on the way!

Exercises
^^^^^^^^^

#. Build a certification object for the ``Auction`` and ``Escrow`` contracts.
