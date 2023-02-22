.. _support_inline_datums_in_constraint_library:

ADR 8: Support inline datums in constraint library
==================================================

Date: 2022-08-14

Author(s)
---------

koslambrou <konstantinos.lambrou@iohk.io>

Status
------

Proposed

Context
-------

In Babbage era (available after the Vasil HF), the Cardano blockchain will support `inline` datums by changing the ``TxOut`` data type.

In Alonzo era, a ``TxOut`` was able to store arbitrary data called the datum.
However, only the hash of the datum was stored, not the actual datum.

With inline datums available in Babbage era, transaction outputs can either contain the hash of the datum or the actual datum.
Thus, we need to adapt our transaction constraint data type (``TxConstraints``) to support this new feature.

Decision
--------

* We will replace the ``Datum`` parameter in ``TxConstraints``'s data constructor ``MustPayToPubKeyAddress`` with ``Plutus.V2.Ledger.Api.OutputDatum``.
  In the offchain implementation of the constraint, we will use this new data constructor parameter to support either adding the datum in the datum witness set (by using the datum lookups to resolve the hash) or inline it in the transaction output.
  In the PlutusV1 on-chain implementation of the constraint, we will return ``False`` if the datum value matches ``OutputDatum Datum`` because the ledger forbids using Babbage era features with PlutusV1.
  The PlutusV2 on-chain implementation of the constraint is trivial.

* We will modify the data constructor interface, on-chain implementation and off-chain implementation of ``MustPayToOtherScript`` similarly to ``MustPayToPubKeyAddress``.

* We will modify the off-chain implementation of the data constructor ``MustSpendScriptOutput`` in order to support inline datums.
  Currently, the script output's datum is added in the transaction's datum witness set.
  However, if the datum is inlined in the script output, then it is already witnessed.
  Therefore, we don't need to add it in the datum witness set.

Argument
--------

The main decision was to find out which data type will replace ``Datum`` in the interface of ``MustPayToPubKeyAddress`` and ``MustPayToOtherScript``.
The decision to use ``Plutus.V2.Ledger.Api.OutputDatum`` was mainly because of the constraint library's main design: the parameters of ``TxConstraints``'s data constructor must work with the on-chain as well as the off-chain implementation.
Therefore, we decided to use ``OutputDatum`` which we know works in on-chain code because this type is used in ``Plutus.V2.Ledger.Api.ScriptContext``.

Notes
-----
