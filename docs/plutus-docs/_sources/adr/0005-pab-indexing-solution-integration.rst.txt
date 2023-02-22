ADR 5: PAB and indexing component integration
=============================================

Date: 2022-07-27

Author(s)
---------

koslambrou <konstantinos.lambrou@iohk.io>

Status
------

Proposed

Context
-------

Let's start with the problematic example (copy-paste of the current `PubKey`
contract in `plutus-use-cases`).

.. code-block:: haskell

  -- | Lock some funds in a 'PayToPubKey' contract, returning the output's address
  --   and a 'TxIn' transaction input that can spend it.
  pubKeyContract
      :: forall w s e.
      ( AsPubKeyError e
      )
      => PaymentPubKeyHash
      -> Value
      -> Contract w s e (TxOutRef, Maybe ChainIndexTxOut, TypedValidator PubKeyContract)
  pubKeyContract pk vl = mapError (review _PubKeyError   ) $ do
      -- Step 1
      let inst = typedValidator pk
          address = Scripts.validatorAddress inst
          tx = Constraints.mustPayToTheScriptWithDatumHash () vl
      ledgerTx <- mkTxConstraints (Constraints.typedValidatorLookups inst) tx
                 >>= submitUnbalancedTx . Constraints.adjustUnbalancedTx

      -- Step 2
      _ <- awaitTxConfirmed (getCardanoTxId ledgerTx)

      -- Step 3
      let refs = Map.keys
                 $ Map.filter ((==) address . txOutAddress)
                 $ getCardanoTxProducedOutputs ledgerTx
      case refs of
          []                   -> throwing _ScriptOutputMissing pk
          [outRef] -> do
              -- Step 4
              ciTxOut <- unspentTxOutFromRef outRef
              pure (outRef, ciTxOut, inst)
          _                    -> throwing _MultipleScriptOutputs pk

Here's an outline of the contract's steps:

1. Creates a transaction and submits it to the node
2. Waits for transaction to be confirmed
3. Finds the first UTXO of that transaction (return type `TxOutRef`)
4. Queries the plutus-chain-index to get the `ChainIndexTxOut` out of that
   `TxOutRef`

The problem is that the `ciTxOut` variable in step 4 will almost always result
in `Nothing`.

Why? Here’s some context.

The PAB listens to the local node and stores blockchain information in memory
such as the status of transactions, the status of transaction outputs, the last
synced slot, the current slot, etc., in a variable of type `BlockchainEnv`.
The `awaitTxConfirmed` is actually querying the state of `BlockchainEnv` and
waits until the status of the transaction transitions to `Confirmed`.

Meanwhile, plutus-chain-index (our main indexing component at the time of this
writing) is also listening to incoming blocks from the local node and indexes
them into a database.
The indexed data can be queried using the REST API interface.

This brings up the main issue: the PAB and plutus-chain-index each listen to
the same source of information (a local Cardano node), but each index the
information at different speeds.
For a dApp developer writing off-chain code using the Contract API, there is no
abstraction for handling multiple sources of truth.

Currently, in the best case scenario (fully synced PAB and plutus-chain-index),
plutus-chain-index will always trail behind the in-memory storage of the PAB by
a few seconds.
Therefore, even in this scenario, querying the plutus-chain-index with
`unspentTxOutFromRef` in the above contract has a high probability of returning
`Nothing`.

Decisions
---------

The best solution is probably a combination of the `Alternative solutions`_
described below.
However, we will mainly choose the `Query functions should interact with a
single source of truth`_ solution.

* We will replace `plutus-chain-index` with `Marconi` as PAB's indexing
  component

* We will move out the blockchain information indexed by PAB in Marconi

* We will add new indexers in Marconi in order to replicate the information
  indexed by `plutus-chain-index`

* We will adapt the architecture of Marconi (which will become our new indexing
  component) to support waiting queries

* Since we suppose that indexing component should be in the same machine as the
  PAB, then we will use Marconi as a library to index and query the indexed
  blockchain information without relying on an HTTP API

Alternative solutions
---------------------

In this section, we describe all the ways to deal with the problem.
Note that final decision might include one or more of these alternate
solutions.

Make sure all components are in sync
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can make sure that all indexing components are syncing at the same speed.
For example, if PAB syncs from the local node and arrives at slot 100, then it
needs to wait for the chain-index to also arrive at slot 100. Only then can it
respond to a Contract request.

A simply way to achieve this behavior is to change the implementation of
Contract API handler functions (like `utxosAt`, `unspentTxOutFromRef`, etc.) by
waiting for the component to be in sync with all other components.
For example, let's take the `utxosAt` request which basically queries UTXOs
from a given address using `plutus-chain-index`.
We could, before querying, wait for `plutus-chain-index` to be in sync with all
components which index blockchain data (PAB, Blockfrost_).

Pros:

- All the indexed information is consistent with each other

Cons:

- As fast as the slowest indexing component

- Tight coupling between the indexing components meaning that if the Contract
  only uses chain-index requests without using requests from other indexing
  components, the chain-index will still have to wait for all other components
  to be in sync with each other

Add indexing specific functions in the Contract API
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In this scenario, we would need to split Contract API requests which interact
with an external indexing component to the ones that use the PAB.
Currently, we have `awaitTxConfirmed` which uses the indexed information in the
PAB to wait for a transaction status to change to `Confirmed`.
On top of that, we can have `awaitTxIndexed` or `awaitTxOutIndexed` which will
wait for the information to be indexed in the external indexing component.

Pros:

- Limits design change on the PAB

- More control given to the user of the Contract API

Cons:

- Adds an undesired complexity to the Contract API

- We'll need to add a bunch of functions (e.g., `currentNodeSlot`,
  `currentMarconiSlot`, `awaitMarconiTxConfirmed, `awaitScrollsTxConfirmed`,
  etc.) for each new indexing component we want to support

Query functions should interact with a single source of truth
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In this scenario, we make the design decision that the Contract API should only
interact with a single indexing component.
Thus, any blockchain information currently stored in the PAB should be moved to
the indexing component.
Also, combining indexing components would need to be integrated in the single
indexing solution that’s connected to the PAB.

Pros:

- Simplest in design to implement (other than manual work to move code)

- No modification to the Contract API

- Augments PAB's cohesion, because it's responsability will be limited to
  contract instance management

Cons:

- The design of the indexing component will need to be changed to support
  waiting queries (like the `awaitTxConfirmed` from PAB)

- Still under the assumption that the indexing component is in sync with the
  PAB in order to use some querying functions

.. ///////////////////////////////////////////////////////////////////////////////////
.. Commented this alternative solution as it would make sense to introduce it
.. as a separate ADR for changing the indexing query interface of the Contract
   API.
.. ///////////////////////////////////////////////////////////////////////////////////
..
.. Alter indexing query APIs to include valid slots information
.. ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. In this scenario, we can instead embrace the fact that the indexing component
.. is not always in sync with the PAB.
.. We can alter the indexing component query APIs (like `utxosAt` or
.. `txOutFromRef`) to include some sort of `ValidSlotRange` information.
.. Therefore, the user would need to specify the `SlotRange` from which he expects
.. the indexing component to be synced.
.. If the currently indexed slot of the PAB and indexing component is not up to
.. what we need, we can either wait for it or throw an error.

.. Pros:

.. - Would work accross different indexers as long as we can get an indication as
..   to the slot number they are synced up to (which is not unreasonable to
..   assume)

.. - Gives users control over how to deal with the synchronization of states when
..   querying

.. Cons:

.. - Makes the query APIs more complicated, which ultimately result in a more
..   complex contract

.. - Makes the contract slower if it has to wait for the indexing component to be
..   synced to the desired `SlotRange`

Implications
------------

Having a single source of truth indexer will augment the PAB's cohesion and
greatly simpy the `plutus-pab` codebase.
Also, we will not encounter the situation where we have multiple indexers that
index at different speeds.

However, there is still the problem that the indexing solution might not be in sync with the local node.
Therefore, we need to make another decision on how to deal with it.

Notes
-----

This problem manifested itself in the Github issue `#473
<https://github.com/input-output-hk/plutus-apps/issues/473>`_ and there was a
temporary fix in the PR `#496
<https://github.com/input-output-hk/plutus-apps/pull/496>`_.
However, the proper solution to the issue would be the implementation of this
ADR.

.. _Blockfrost: https://blockfrost.io
.. _Scrolls: https://github.com/txpipe/scrolls

This ADR has been discussed here: `#550 <https://github.com/input-output-hk/plutus-apps/pull/550>`_.
