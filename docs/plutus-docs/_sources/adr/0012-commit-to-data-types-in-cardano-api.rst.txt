.. _commit-to-data-types-in-cardano-api:

ADR 12: Commit to data types in cardano-api
===========================================

Date: 2022-10-03

Author(s)
---------

koslambrou <konstantinos.lambrou@iohk.io>

Status
------

Draft

Context
-------

Since the genesis of the ``plutus-apps`` repository, the components have been
historically using the types in the ``plutus-ledger-api`` package (which is now
part of the ``plutus`` repository) in the off-chain part of a Plutus application.

This was desirable in order to start designing a way to build Plutus applications before the ``cardano-ledger`` actually supported the Alonzo-era features.
Of course, this resulted in the unintended consequence that we used ``TxInfo`` types (types that are designed to be used in Plutus scripts) in off-chain code.
This wouldn't have been a problem if there was a 1:1 relationship between on-chain and off-chain types.
However, that presumption is wrong.

Let's take the example of the ``TxOut`` representation of ``plutus-ledger-api`` for ``PlutusV2``.

.. code-block:: haskell

  data TxOut = TxOut {
    ...
    txOutReferenceScript :: Maybe ScriptHash
    }

As we can see, the ``TxOut`` can optionally store the ``ScriptHash`` of the referenced script.
However, that is *not* the adequate representation of a ``TxOut`` in a
transaction given the ``cardano-ledger`` specification.
The off-chain ``TxOut`` should instead be:

.. code-block:: haskell

  data TxOut = TxOut {
    ...
    txOutReferenceScript :: Maybe Script
    }

where the reference script field can store that *actual* script, not just the hash.

This proved that we need to start moving away from `plutus-ledger-api` types in
the off-chain part of Plutus applications, especially in components like the
emulator and the chain indexer.

Decision
--------

* We will create a ``cardano-api-extended`` cabal project, which will contain
  features and utilities on top of the ``cardano-api`` package.
  A similar idea has emerged with hydra-cardano-api_.
  This package will contain:

    * type synonyms for working with the latest era

    * a simplified and working transaction balancing function (mainly the
      ``Ledger.Fee.makeAutoBalancedTransaction`` in ``plutus-apps``)

    * validation rules (most of what's in the current ``Ledger.Validation``)

  The ``cardano-api-extended`` package will re-export the modules from the
  hydra-cardano-api_ package which contain type synonyms for working with the
  latest era.

* We will remove our data type representation of a Cardano transaction
  (``Ledger.Tx.Internal.Tx`` in ``plutus-ledger``) and fully commit to
  ``Cardano.Api.Tx.Tx era`` (or ``Cardano.Api.Tx.TxLatestEra``) in the codebase.

* We will replace any use of ``plutus-ledger-api`` types by ``cardano-api`` and
  ``cardano-api-extended`` types whenever we work with the off-chain part of
  Plutus applications.
  For instance, the ``plutus-contract`` emulator and types in the
  ``Plutus.Contract.Request`` module of ``plutus-contract`` will be updated to
  use ``cardano-api`` types.
  However, the data types in ``Ledger.Constraints.TxConstraints`` will continue
  to use ``plutus-ledger-api`` types because the constraints are used to
  generate both Plutus scripts and transactions.
  Therefore, there should be no breaking change on the API for writing Plutus
  applications.

* We will improve ``cardano-api`` through ``cardano-api-extended`` and regularly
  push changes upstream when possible.

* We will restructure the ``Ledger.Tx.CardanoApi`` module in ``plutus-ledger``
  and move functions in ``cardano-api-extended``.

* We will enhance the ``plutus-contract`` emulator by being able to balance and
  submit ``cardano-api`` transactions.

* We will modify the ``plutus-contract`` emulator to fully use the
  ``cardano-ledger`` transaction validation rules, and we will remove our custom
  validation rules (module ``Ledger.Index`` in ``plutus-ledger``).

Argument
--------

The ``cardano-api`` package is expected to be the supported entry point for
clients to interact with Cardano chain in Haskell in the foreseeable future.
Therefore, we should extensively use this package and upstream changes as much
as possible so that users not using the packages in ``plutus-apps`` can still
have a good experience writing Plutus applications using ``cardano-api``.

The main arguments for creating the ``cardano-api-extended`` package instead of
using hydra-cardano-api_ directly are:

* faster iterative development for extending ``cardano-api``

* existing plans to upstream the hydra-cardano-api_ in ``cardano-api``

Alternatives
------------

Define our own data types for off-chain use
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This is currently what the ``plutus-apps`` repository is partially doing.
The main problem is that this requires significant maintenance work, especially
when the cardano-ledger specification changes between eras.

Implications
------------

* This decision *should not* impact the user-facing API of our libraries.
  All the changes should be internal.
  Changes to the public-facing API should be part of a separate ADR.

* Any orphan instances that we currently have in ``plutus-ledger`` will need to
  be moved to ``cardano-api-extended``.

* The Marconi-related packages will need to work with ``cardano-api`` types
  instead of ``plutus-ledger-api`` types, as Marconi is a full off-chain
  component.
  This implies removing the ``plutus-ledger`` dependency that we currently have
  in ``marconi``.

Notes
-----

.. _hydra-cardano-api: https://github.com/input-output-hk/hydra-poc/tree/master/hydra-cardano-api

