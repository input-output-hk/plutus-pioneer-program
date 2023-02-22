.. highlight:: haskell
.. _basic_apps_constraints_tutorial:

Extending the basic Plutus app with the constraints API
=======================================================

The previous tutorial (see :ref:`basic_apps_tutorial`) showed you how to write a Plutus app that locks some Ada in a script output and splits them evenly between two recipients.
In this tutorial, we will reuse the same example, but we will use instead the constraints API which will be used to generate the on-chain and off-chain part of the Plutus app.
This will allow your application to create a transaction which is *mostly* consistent with the validator function.

Given a `SplitData`, let's start by defining a function which generates the constraints to unlock funds locked by the split validator.

.. literalinclude:: BasicAppConstraints.hs
   :start-after: BLOCK1
   :end-before: BLOCK2

With the constraints, let's start by defining the validator function.

.. literalinclude:: BasicAppConstraints.hs
   :start-after: BLOCK2
   :end-before: BLOCK3

As you can see, it's much simpler than the original version.

Now to the off-chain part.
The `lock` endpoint doesn't change.
However, we can change the `unlock` endpoint to use the constraints we defined above.

.. literalinclude:: BasicAppConstraints.hs
   :start-after: BLOCK4
   :end-before: BLOCK5

That's it! The rest of the contract is the same as the previous tutorial.
