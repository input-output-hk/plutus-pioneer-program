.. _tx_validity_time_range_fix:

ADR 13: Transaction validity time range fix
===========================================

Date: 2022-10-19

Author(s)
---------

koslambrou <konstantinos.lambrou@iohk.io>

Status
------

Draft

Context
-------

The following code samples were executed with ``cabal repl plutus-ledger`` on the plutus-apps commit
hash `172873e87789d8aac623e014eff9a39364c719ae <https://github.com/input-output-hk/plutus-apps/commit/172873e87789d8aac623e014eff9a39364c719ae>`_.

Currently, the ``plutus-ledger-constraint`` library has the ``MustValidateIn`` constraint which

1) validates that a given ``POSIXTimeRange``` contains the ``TxInfo``'s validity range
2) creates a transaction with the provided ``POSIXTimeRange``

The implementation of 1) is trivial.
However, a major issue arises for the implementation of 2).
Setting the validity interval of a Cardano transaction is done by specifing the slot of the lower
bound and the slot of the upper bound.
Therefore, the ``MustValidateIn`` constraint needs to convert the provided ``POSIXTimeRange`` to
essentially a ``(Maybe Slot, Maybe Slot)``.
The problem is that there are many ways to convert a ``POSIXTime`` to a ``Slot``.

Currently, provided a ``POSIXTimeRange``, ``plutus-contract`` does the following:

* convert the time range to a slot range with ``Ledger.TimeSlot.posixTimeRangeToContainedSlotRange :: POSIXTimeRange -> SlotRange``

* convert the ``SlotRange`` to ``(Cardano.Api.TxValidityLowerBound,
  Cardano.Api.TxValidityUpperBound)`` (essentially a ``(Maybe Slot, Maybe Slot)``)

The issue with these conversion is that the ``POSIXTimeRange`` and ``SlotRange`` intervals are
type synonyms of the ``PlutusLedgerApi.V1.Interval.Interval a`` datatype which has has a "Closure"
flag for each of the bounds.

Therefore, the conversions yields a discrepency when `cardano-ledger` converts the
``(Cardano.Api.TxValidityLowerBound, Cardano.Api.TxValidityUpperBound)`` to a ``POSIXTimeRange``
when creating the ``TxInfo``.

Let's show some examples to showcase the issue.

.. code-block:: haskell

  > let sc = SlotConfig 1000 0
  > let interval = (Interval (LowerBound (Finite 999) False) (UpperBound PosInf True))
  > let r = posixTimeRangeToContainedSlotRange sc interval
  > r
  Interval {ivFrom = LowerBound (Finite (Slot {getSlot = 0})) False, ivTo = UpperBound PosInf True}
  > let txValidRange = toCardanoValidityRange r
  > txValidRange
  Right (TxValidityLowerBound ValidityLowerBoundInBabbageEra (SlotNo 1),TxValidityNoUpperBound ValidityNoUpperBoundInBabbageEra)

When creating the ``TxInfo``, ``cardano-ledger`` will convert the previous ``cardano-api`` validity slot range to:

.. code-block:: haskell

  (Interval (LowerBound (Finite 1000) True) (UpperBound PosInf True))

In practical reasoning, ``LowerBound (Finite 999) False`` and ``LowerBound (Finite 1000) True`` are
equal considering the precision of 1000 milliseconds per slot.
However, given ``Interval`` semantics, these are not the same values.
Therefore, if the constraint ``mustValidateIn interval`` is used both to create a transaction and
inside a Plutus script (corresponds to the check ``interval `contains` txInfoValidRange
scriptContextTxInfo``), then the Plutus script will yield ``False``.

We can identify a similar behavior with the upper bound.

.. code-block:: haskell

  > let sc = SlotConfig 1000 0
  > let interval = (Interval (LowerBound NegInf True) (UpperBound (Finite 999) True))
  > let r = posixTimeRangeToContainedSlotRange sc interval
  > r
  Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound (Finite (Slot {getSlot = 0})) True}
  > let txValidRange = toCardanoValidityRange r
  > txValidRange
  Right (TxValidityNoLowerBound,TxValidityUpperBound ValidityUpperBoundInBabbageEra (SlotNo 1))

When creating the ``TxInfo``, ``cardano-ledger`` will convert the previous ``cardano-api`` validity slot range to:

.. code-block:: haskell

  (Interval (LowerBound NegInf True) (UpperBound (Finite 1000) False))

Again, a Plutus script with ``interval `contains` txInfoValidRange scriptContextTxInfo`` will yield ``False``.

Additionnaly, the current behavior makes it hard to reason about how a ``POSIXTime`` gets translated
into a ``Slot`` when creating a transaction.
Ultimately, a DApp developer should have control over how his ``POSIXTime`` gets translated to a
``Slot``.

Decision
--------

* We will create the following datatype:

  .. code-block:: haskell

    -- | ValidityInterval is a half open interval. Closed (inclusive) on the bottom, open
    -- (exclusive) on the top. A 'Nothing' on the bottom is negative infinity, and a 'Nothing'
    -- on the top is positive infinity.
    data ValidityInterval a = ValidityInterval
      { invalidBefore :: !(Maybe a) -- ^ Inclusive lower bound or negative infinity
      , invalidHereafter :: !(Maybe a) -- ^ Exclusive upper bound or positive infinity
      }

* We will add the following constraint and smart constructor:

  .. code-block:: haskell

    data TxConstraint =
      ...
      MustValidateInTimeRange !(ValidityInterval POSIXTime)

    mustValidateInTimeRange :: !(ValidityInterval POSIXTime) -> TxConstraints

* We will remove the ``MustValidateIn`` constraint and deprecate the the ``mustValidateIn`` smart
  constructor which will be replaced by ``mustValidateInTimeRange``.

* We will create the smart constructor

  .. code-block:: haskell

    mustValidateInSlotRange :: !(ValidityInterval Slot) -> TxConstraints

  which will translate the provide validity slot range into a ``POSIXTimeRange`` using
  ``Ledger.TimeSlot.posixTimeRangeToContainedSlotRange``.

Argument
--------

* The new ``mustValidateInTimeRange`` constraint will solve the discrepency between the way the
  validity constraint range converts a ``POSIXTime`` to a ``Slot`` and how ``cardano-ledger``
  converts the ``Slot`` to ``POSXITime`` when creating the ``TxInfo``.

* However, it won't solve the issues when the provided ``POSIXTimeRange`` is not an unit of 1000
  milliseconds.
  For this scenario, we provide the ``mustValidateInSlotRange`` which will always create
  ``POSIXTimeRange`` that is an unit of 1000 milliseconds.

* Another benefit of the ``mustValidateInSlotRange`` constraint is to give control to the users on
  how to convert their times in ``POSIXTime`` to a ``Slot``.

Implications
------------

* We will have to update the ``plutus-use-cases`` examples to use ``mustValidateInSlotRange`` when
  creating transactions, but still use ``POSIXTime`` or ``POSIXTimeRange`` when defining the
  parameters (inputs) of the use cases.
  Same for end-users.

Alternatives
------------

Add ``MustValidateInSlotRange`` constraint
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If we decide to go in the direction of only specifying slots when creating transaction, then a
logical solution would be replace the ``MustValidateInTimeRange`` constraint by
``MustValidateInSlotRange (Maybe Slot) (Maybe Slot)``.
However, the main issue with this solution is that this constraint would not work in a Plutus
script, because there is no way to convert the ``POSIXTimeRange`` validity range of a ``TxInfo`` to
a ``(Maybe Slot) (Maybe Slot)``.

Remove ``mustValidateInTimeRange``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

By defining ``mustValidateInSlotRange``, we could decide to completly remove
``mustValidateInTimeRange`` and force users to work with slots.
However, unless we get clear feedback from end-users, we will keep ``mustValidateInSlotRange`` until
new evidence says otherwise.

Alter ``mustValidateInTimeRange``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Another alternative solution would be to keep ``mustValidateInTimeRange``, but with additonnal
parameters which would specify how to convert the ``(Maybe POSIXTime, Maybe POSIXTime)`` to a
``(Maybe Slot, Maybe Slot)``.
For example, given the lower (or upper) bound of the ``POSIXTimeRange``, do we convert it to the
closest slot?
Or do we convert it to the lower (or upper) bound slot that includes the ``POSIXTime``?
This can potentially be discussed in a future ADR if there is value for end-users.

Notes
-----

This ADR is motivied by the SealedBidAuction bug fix in the PR `#767 <https://github.com/input-output-hk/plutus-apps/pull/767>`_.

This ADR has been implemented here: `#878 <https://github.com/input-output-hk/plutus-apps/pull/878>`_.
