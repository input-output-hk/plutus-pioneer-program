.. _support_return_and_total_collateral_when_building_transactions:

ADR 11: Support return and total collateral when building transactions
======================================================================

Date: 2022-08-30

Author(s)
---------

koslambrou <konstantinos.lambrou@iohk.io>

Status
------

Proposed

Context
-------

In Babbage era (available after the Vasil HF), Cardano transactions will contain new collateral related fields: "return collateral" and "total collateral" collateral.
Return collateral (also called "collateral output") and total collateral are detailed in `CIP-40 <https://cips.cardano.org/cips/cip40>`_.

In summary, return collateral is a special output (basically of type ``TxOut``) that becomes available in case there is a failed phase-2 validation.
In addition, we have the new total collateral field which explicitly says how much collateral (in lovelace) is going to be actually consumed in the case of phase-2 validation failure.

Decision
--------

* We will add the ``txReturnCollateral`` and the ``txTotalCollateral`` fields in the `Ledger.Tx.Internal.Tx` data type.

* We will modify the ``Wallet.Emulator.Wallet.handleBalance`` function in `plutus-contract` to set the correct return and total collateral for an ``UnbalancedTx`` (of type ``Either CardanoBuildTx EmulatorTx``).
  In either type of transaction, we would compute the ``txTotalCollateral`` while estimating the fee with the formula ``quot (txfee txb * (collateralPercent pp)) * 100`` and then set ``txReturnCollateral`` with the formula ``sum collateralInputs - txTotalCollateral``.

Argument
--------

As the user would want to pay the least amount of collateral, we made the decision to modify the balancing algorithm to automatically set the return collateral to the highest possible value.

Alternatives
------------

The main alternative would have been to add a new constraint such as ``MustReturnCollateral TxOut`` in the constraints library to allow the users to specify the return collateral themselves.
However, as explained in the `Argument`_ section, users would always want to pay the least amount of collateral.
Therefore we don't expect the need to set the return collateral manually.

Notes
-----

