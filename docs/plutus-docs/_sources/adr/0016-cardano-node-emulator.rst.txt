.. _cardano_node_emulator:

ADR 16: Self-contained cardano-node emulator component
======================================================

Date: 2022-11-23

Author(s)
---------

koslambrou <konstantinos.lambrou@iohk.io>

Status
------

Draft

Context
-------

The current ``plutus-contract`` Haskell package allows developers to write Plutus applications using
the ``Contract`` Monad.
On top of that, the package contains a Plutus application contract emulator, a way to run those
contracts on a simulated environment so that they can be tested.
The emulator in question contains multiple components like the testing framework (including the
``ContractModel``), the wallet emulator, a chain-index emulator and the node emulator.::

     +-------------------+     +-------------------+     +-----------------+
     | Testing framework |<----+ Contract Emulator +---->| Wallet emulator |
     +-------------------+     +-----+--------+----+     +--------+--------+
                                     |        |                   |
                                     |        |                   v
    +----------------------+         |        |           +---------------+
    | Chain index emulator |<--------+        +---------->| Node emulator |
    +----------------------+                              +---------------+


The main reason we can't use a real wallet or node backend is because they are not fast enough to be
able to run many test cases using property tests with the ``ContractModel``.

Now, we believe the node emulator to be a useful separate component that other testing framework can
leverage for being able to write fast test cases.

Decision
--------

* We will create a new Haskell package named ``cardano-node-emulator``.

* We will move node related functionality into this new package.
  Here are some modules (or parts of a module) that will need to be moved over.

  * The ``Ledger.Validation`` module which validates transactions using ``cardano-ledger`` should
    only be used by ``cardano-node-emulator`` and should not be exposed.

  * The ``Ledger.Fee`` module, which calculates the fees for a given transaction, should be internal
    to ``cardano-node-emulator``.

  * The ``Ledger.Generators`` module, which contains generators for constructing blockchains and
    transactions for use in property-based testing, should be internal to ``cardano-node-emulator``.

  * The ``Ledger.TimeSlot.SlotConfig`` datatype should only be used by the
    ``cardano-node-emulator``.
    An end user should not use this representation in a real world scenario.
    See :ref:`time_conversion_semantic_change` for more details.

  * The ``Wallet.Emulator.Chain`` module in ``plutus-contract`` should be moved in
    ``cardano-node-emulator``.

  * The ``Ledger.Params`` which allows to configuration the network parameters should be moved over
    to ``cardano-node-emulator``.

Argument
--------

Splitting out the node emulator in a separate Haskell component allows to better scope it's
dependency footprint.
We don't want the list of dependencies to be too large in order to keep it as a lightweight
component.

Notes
-----

This ADR is addressed in PR `#831 <https://github.com/input-output-hk/plutus-apps/pull/831>`_.

