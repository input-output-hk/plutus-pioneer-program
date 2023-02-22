.. _support_reference_scripts_in_constraint_library:

ADR 9: Support reference scripts in constraint library
======================================================

Date: 2022-08-15

Author(s)
---------

koslambrou <konstantinos.lambrou@iohk.io>

Status
------

Accepted

Context
-------

In Babbage era (available after the Vasil HF), the Cardano blockchain will support "reference scripts" by changing the ``TxOut`` data type.
Reference scripts are used to attach arbitrary scripts to transaction outputs and are used to satisfy script requirements during validation, rather than requiring the spending transaction to do so.
Thus, we need to adapt our transaction constraint data type (``TxConstraints``) to support this new feature.

Decision
--------

* We will add ``Maybe ScriptHash`` as a new data constructor parameter for the constraints ``MustPayToPubKeyAddress``, ``MustPayToOtherScript``, ``ScriptOutputConstraint`` in ``TxConstraints``.
  In the off-chain implementation of those constraints, if a reference script hash is provided, we will need to find the actual script in the lookups table so that we can include it in the transaction output.
  In the PlutusV1 on-chain implementation of the constraint, we will return ``False`` if a reference script is provided because the ledger forbids using Babbage era features with PlutusV1.
  The PlutusV2 on-chain implementation of the constraint is trivial.

* We will modify the off-chain implementation of ``MustSpendScriptOutput`` and ``ScriptInputConstraint`` in order to add support for witnessing a script by actually providing it, or by pointing to the reference input which contains the script.

Argument
--------

The main decision was to find out which data type will represent reference scripts.
Similarly to :ref:`support_inline_datums_in_constraint_library`, the decision to use ``Maybe ScriptHash`` was mainly because of the constraint library's main design that data types need to work with on-chain as well as off-chain implementation.

Notes
-----

This ADR has been addressed in PR `#662 <https://github.com/input-output-hk/plutus-apps/pull/662>`_, `#666 <https://github.com/input-output-hk/plutus-apps/pull/666>`_ and `#678 <https://github.com/input-output-hk/plutus-apps/pull/678>`_
