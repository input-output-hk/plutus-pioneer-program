.. _support_reference_inputs_in_constraint_library:

ADR 7: Support reference inputs in constraint library
=====================================================

Date: 2022-08-09

Author(s)
---------

koslambrou <konstantinos.lambrou@iohk.io>

Status
------

Accepted

Context
-------

After the Vasil HF, the Cardano blockchain will support `reference` inputs by adding a new field in the transaction data type.
With reference inputs, transactions can take a look at UTXOs without actually spending them.

Thus, we need to adapt our transaction constraint data type (``TxConstraints``) to support referencing UTXOs.

Decision
--------

* We will add the data constuctor ``MustReferenceOutput TxOutRef`` to the ``TxConstraints`` data type.

* The PlutusV1 on-chain implementation of this new constraint will simply return ``False``.
  However, `cardano-ledger` throws a phase-1 validation error if transactions that use the some of the new features (reference inputs, inline datums and reference scripts) try to execute PlutusV1 scripts.
  See the `Babbage era ledger specification <https://hydra.iohk.io/job/Cardano/cardano-ledger/specs.babbage-ledger/latest/download-by-type/doc-pdf/babbage-changes>`_.
  Therefore, the only way to get a phase-2 validation error would be to use this constraint on-chain in a PlutusV1 script, without using any of the new Babbage era features off-chain.

* The PlutusV2 on-chain implementation of this new constraint will check that the provided ``TxOutRef`` is part of the ``ScriptContext``'s reference inputs.

Argument
--------

At first glance, we might think that we need two data constructors for reference inputs such as ``MustReferencePubKeyOutput`` and ``MustReferenceScriptOutput`` in contrast to the existing ``MustSpendPubKeyOutput`` and ``MustSpendScriptOutput`` constraints.
However, we do not need to make the distinction between public key outputs and script outputs because we're not spending the output, therefore, we don't need to provide a redeemer nor the actual script as a witness to the transaction input.

Notes
-----

This ADR has been addressed in the PRs `#640 <https://github.com/input-output-hk/plutus-apps/pull/640>`_ and `#661 <https://github.com/input-output-hk/plutus-apps/pull/661>`_.
