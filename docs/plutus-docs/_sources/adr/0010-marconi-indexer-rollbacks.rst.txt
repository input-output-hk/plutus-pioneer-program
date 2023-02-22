.. _marconi_indexer:

ADR 10: Rolling back on-disk data for Marconi indexers
======================================================

Date: 2022-08-05

Author(s)
---------

Ometita Radu <radu.ometita@iohk.io>

Status
------

Draft

Context
-------

Off-chain code needs access to indexed portions of the blockchain. Currently, we have a working solution in the form of the chain-index and PAB (which both index information). The big problem with the current solution is its lack of reuse (or modularity) capability.

We attempt to fix that problem with Marconi where we currently have a generic indexer that stores volatile information in memory and blocks that have been fully committed (are old enough to guarantee that they will not be rolled back) on disk. The K blockchain parameter represents how many blocks deep the blockchain becomes immutable (no rollbacks can occur beyond K blocks).

We currently need to keep K blocks in memory to be able to perform rollbacks. However, the K parameter can be adjusted for indexers which land us in the unfortunate position of saying that there may be data corruption in the case where the number of rollbacked blocks is larger than the number of blocks stored in memory. While this is both detectable and unlikely to happen we think that our current solution can prevent it without any significant drawbacks.

Decision
--------

After receiving feedback on the initial implementation of the PAB and chain-index we needed a generic way of indexing information where we can control the amount of memory the indexer uses. The first version of indexers store the volatile blocks in memory and persist them to disk whenever they become older than the K parameter.

We make a distinction between the volatile blocks which are stored in memory as events (and are derived from blocks). We fold these events into the aggregated on-disk data structure for which we do not require to keep multiple versions (rollbacks cannot happen for this data structure).

We improve on that idea by allowing part of the volatile blocks to be stored on disk. While this is not required at the API level, the usage pattern would be to have a set of events, as well as the aggregated data structure stored on disk. The compromise here is that the more data is on the disk, the more we will need to work with the disk and the slower the indexing process will become. The advantage would be reduced memory usage.

Events
^^^^^^

Events need to contain information about the slot number and block id when they were produced.

The slot number information is used in case of rollbacks (when we only get the old slot number that we need to rollback to) and for resuming the operation of the index, in which case we need both the slot numbers and block ids.

Note that to support resume from disk we need to always have at least one event persisted on disk which contains the slot number and block id from which we are supposed to resume operation. In case there are more than one events stored on disk we can use those as resume alternatives in the case of the ChainSync protocol.

Queries
^^^^^^^

We also extend the queries with a bit more structure that will make specifying query validity intervals possible. The validity is important in the case where we want to query several indexers and we would like them to require to have processed all the information up to some slot number or be between some slot interval.

At this point all query results are synchronous. We have plans to extend this functionality, but these plans are based on updating the notification system for indexers which will be described in a further ADR.

API Design
^^^^^^^^^^

We want the API for our users to be as flexible as possible, so some of our previously mentioned design patterns are not captured by the API, but rather by its implementation.

Data types
""""""""""

* The `Events` data type contains the following fields:

  * Slot numbers (a data type that supports ordering)
  * Block id (a data type that supports equality checking)
  * e (type variable standing for the event)

* The `Query` data type contains the following fields:

  * Validity interval (can be any interval defined by using the slot numbers or a special value that turn off checking for validity)
  * q (type variable standing for the query)

* Result

  * Slot number at which the query was ran
  * r (type variable standing for the query result)

Functions
"""""""""

* The `Query` function takes the following parameters:

  * Indexer - The indexer that we are using to run the query.
  * Validity interval - The interval under which the query needs to be ran.
  * The query (q type variable) - The user-defined query.
  * The query result

* The `Store` function takes the following parameters:

  * Indexer - The indexer for which we run the function
  * Does not return anything useful

* The `Resume` function takes the following parameters:

  * Indexer - The indexer we need to query for the last consumed slot numbers
  * Returns a list of slot numbers and block ids

Runtime parameters
""""""""""""""""""

* Minimum events retained (this should be the previously mentioned K parameter)

  Currently the main cardano network guarantees that there will be no rollbacks beyond 2160 blocks. This would be that parameter.

* Maximum in-memory events (should be less than K)

  How many events do we want to keep in memory. For the main network this should be any number lower that 2160. The larger the number the less frequent writing to disk is and the more RAM is used.

Extension mechanisms
""""""""""""""""""""

A. Storage engine

You can customise the query and store functions which run in some generic monad to use whatever backend is best for the job. We currently use SQLite, but that is more for convenience than anything else.

B. Query intervals

If you want to specify an interval for your queries (which is highly encouraged) then you need to have in memory (or on disk) sufficient information to reconstruct the state at the given slot number. The information required is contained in the event (which includes the slot number). By storing more than K events you can extend the query interval as much as you need. In extreme, you can store events without ever aggregating and deleting them, in which case your queries can span the whole blockchain.

Implementation
^^^^^^^^^^^^^^

This is the way we suggest people implement storage for the indexers::

  | Memory |       Disk         |
  |--------|--------|-----------|
  | Events | Events | Aggregate |

To support the resume function we need to always have at least one event stored on disk. This is an invariant that an implementation can keep by ensuring that the number of in-memory events is less than K.

Since the number of events stored in memory is constant we can keep on using a ring buffer backed by the vector library.

Events are moved into storage whenever the in-memory buffer becomes full. When they are moved into storage we also need to decide what we are folding into the stored aggregated data structure. We should never fold any events that are newer than K blocks.

We suggest using type families for the implementation due to the functional dependencies between the handler type and the monad that the indexer runs in, as well as the dependency between the query type and the result type (and in the future the notification type).
