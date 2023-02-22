.. _time_conversion_semantic_change:

ADR 15: Time conversion semantic change
=======================================

Date: 2022-11-19

Author(s)
---------

koslambrou <konstantinos.lambrou@iohk.io>

Status
------

Draft

Context
-------

Currently, PAB users need to provide the ``SlotConfig`` in the configuration file, which is passed
through to the ``Contract`` API, which users can use to convert between a ``Slot`` and a ``POSIXTime``.
However, the current ``SlotConfig`` representation supposes that the slot length is the same for all
epochs in the Cardano blockchain, which is not the case.
For example, during the Byron era, the slot length was 20s, while from Shelley era and onwards, the
slot length is 1s.
Therefore, the functions from the ``Ledger.TimeSlot`` module in ``plutus-ledger`` do not compute the
conversion between ``Slot`` and ``POSIXTime`` the right way.
The current easiest way to compute the time conversions is to query the local Cardano node on the
consensus layer, which requires the ``ouroboros-consensus`` dependency.

Decision
--------

* We will deprecate the ``Ledger.TimeSlot.SlotConfig`` type and all functions in the
  ``Ledger.TimeSlot`` module using the ``SlotConfig``. The only viable functions are the ones that
  convert between ``Data.Time`` types and plutus types (types related to ``TxInfo``).

* We will copy the ``Ledger.TimeSlot`` module in the emulator (ideally rename it) and keep it as
  an internal module. Any functions not used by the emulator will be removed.

* We will move the ``Ledger.Params`` module inside the emulator as an internal module and modify the
  ``Params`` datatype name to ``EmulatorParams``.

* We will modify the ``Plutus.Contract.Request.getParams`` function to
  ``Plutus.Contract.Request.getProtocolParameters``. This implies modifying the name of Contract
  effect ``GetParamsReq/GetParamsResp``.

* We will create two pairs of effects in ``Plutus.Contract.Effect``:

  .. code-block:: haskell

    data PABReq =
      ...
      | SlotToUTCTimeIntervalReq SlotNo
      | UTCTimeToSlotReq UTCTime
      ...

     data PABResp =
      ...
      | SlotToUTCTimeIntervalResp (UTCTime, SlotLength) -- An alternative can be (UTCTime, UTCTime)
      | UTCTimeToSlotResp SlotNo
      ...

* We will implement the emulator effect interpreter by simply using the ``SlotConfig`` for the conversions.

* We will implement the PAB effect interpreter by using the local node. There are multiple steps to
  implement this:

  * At startup, the PAB will query the ``EraHistory`` from the local node and store it in its local
    environment.
  * We will implement the PAB interpreter by using the ``EraHistory`` alongside the consensus
    functions ``wallclockToSlot`` and ``slotToWallclock``. Here's an example function of how to use
    them:

    .. code-block:: haskell

      -- Calculate slot number which contains a given timestamp
      utcTimeToSlotNo
        :: SystemStart
        -> EraHistory CardanoMode
        -> Time.UTCTime
        -> Either PastHorizonException SlotNo
      utcTimeToSlotNo systemStart (EraHistory _ interpreter) time = do
        let relativeTime = toRelativeTime systemStart time
        (slotNo, _, _) <- interpretQuery interpreter $ wallclockToSlot relativeTime
        pure slotNo

      slotStart
        :: SystemStart
        -> EraHistory CardanoMode
        -> SlotNo
        -> Either PastHorizonException Time.UTCTime
      slotStart systemStart (EraHistory _ interpreter) slotNo = do
        (relativeTime, _) <- interpretQuery interpreter $ slotToWallclock slotNo
        pure $ fromRelativeTime systemStart relativeTime

    However, we will also add an additional step. If the conversion returns ``PastHorizonException``,
    then there is a good probability that the ``EraHistory`` is out of date.
    The reason is that ``EraHistory`` only encodes era information from the moment the user ran the
    query and it cannot predict the future.
    In that case, if the ``PastHorizonException`` is returned, we will re-query the ``EraHistory``
    of the local node, replace the old value in the PAB environment, and retry the conversion.
    If it fails again, we return the error message.

Argument
--------

The solution to create new effects in ``Plutus.Contract.Effects`` has the nice property that the
implementation can differ depending on the environment.
It allows us to keep using ``SlotConfig`` in the emulator, while using the existing implementation
provided by ``ouroboros-consensus`` when using a real Cardano node.

Implications
------------

* We will have to update the ``plutus-use-cases`` examples to use those new conversion functions.
  The user will **not** use ``SlotConfig`` to convert between slots and UTC time. He will
  instead need to use the new effects defined by the ``Contract`` API.

Alternatives
------------

Changing the representation of ``SlotConfig`` to the correct one
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A good solution would be to reuse the ``Summary`` datatype from ``ouroboros-consensus`` which has
the correct representation.
However, the emulator does not depend on ``ouroboros-consensus`` and adding it would incur a large
dependency footprint for such a simple need.
Additionnaly, consensus doesn't (and doesn't want to) expose the ``Summary``, which is internal to
the ``Interpreter`` datatype, which in turn is returned by ``cardano-api`` when querying the local
node for the ``EraHistory``.
Thus, even if we copy-pasted the ``Summary`` datatype in the emulator, we would still need to find
a way to query the ``Summary`` of ``ouroboros-consensus`` and convert it to our own ``Summary``.

The ideal solution would be coordinate with the maintainers of ``ouroboros-consensus`` to move out
the ``Summary`` datatype in a (small) consensus core module and find a way to reconstruct the
``Summary`` when querying the local node.
This should be done in the long term, but it is not our current focus.

Directly use EpochInfo in the emulator and Contract API
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Another thought of solution would be to replace the use of ``SlotConfig`` which ``EpochInfo``.
However, we need ``EpochInfo`` to be an instance of ``FromJSON/ToJSON``, which is not possible
because its data contructor parameters are functions.

Notes
-----

