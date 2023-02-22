.. _end_to_end_testing_strategy:

ADR 17: End-to-end testing strategy for Plutus, cardano-ledger-api and cardano-node-client
==========================================================================================

Date: 2022-12-02

Author(s)
---------

james-iohk <james.browning@iohk.io>

Status
------

Draft

Context
-------

End-to-end testing of `Plutus Core <https://github.com/input-output-hk/Plutus/>`_ functionality is
currently performed by a combination of automation and exploratory approaches, both are performed
on public `preview` and `pre-prod` testnets using a real node.
Automation test scenarios for Plutus are currently being run as part of the wider `cardano-node-tests
<https://github.com/input-output-hk/cardano-node-tests/>`_ test suite, which uses a Python wrapper
for ``cardano-cli``.
Those tests focus on general ledger/node/cli functionality and only cover a few key scenarios for
Plutus functionality, such as TxInfo and SECP256k1 builtins.

There is also ongoing development work to separate the functionality of `cardano-api
<https://github.com/input-output-hk/cardano-node/tree/master/cardano-api>`_ out into two packages:

* `cardano-ledger-api
  <https://github.com/input-output-hk/cardano-ledger/tree/master/libs/cardano-ledger-api>`_ handles
  the building and balancing of transactions.

* ``cardano-node-client`` will live in `cardano-node
  <https://github.com/input-output-hk/cardano-node>`_ and handle the submitting of balanced
  transactions and querying the ledger state.

Both of these packages are in early stage development and will require end-to-end test coverage.

This document outlines the decisions and arguments for an **additional** approach to end-to-end test
automation using a framework written in Haskell.

The exploratory testing approach is not in the scope of this document.

Decision
--------

* We will create a new end-to-end testing framework written in Haskell called ``plutus-e2e-tests`` that
  will initially be a package in `plutus-apps <https://github.com/input-output-hk/Plutus-apps/>`_,
  see `argument 1`_.

* We will use `cardano-testnet
  <https://github.com/input-output-hk/cardano-node/tree/master/cardano-testnet/>`_
  for configuring and initialising local test network environments, see `argument 2`_.

* We will initially use ``cardano-api`` for building and balancing transactions, and to submit
  balanced transactions and for querying the ledger, see `argument 3`_.

* When available, we will use ``cardano-ledger-api`` instead of ``cardano-api`` for building and
  balancing transactions, see `argument 4`_.

* When available, we will use ``cardano-node-client`` instead of ``cardano-api`` to submit balanced
  transactions and for querying the ledger state to make test assertions, see `argument 5`_.

* We will prioritise ``Plutus`` test coverage over ``cardano-node``, see `argument 6`_.

* We will start by creating a few tests with the node/ledger apis without depending on ``plutus-apps``
  and then assess whether we want to use the Contract API and other off-chain tooling going forwards,
  see `argument 7`_.

* We will continue adding a subset of Plutus tests to ``cardano-node-tests``, see `argument 8`_.

Types of Plutus tests for the ``plutus-e2e-tests`` Haskell framework
--------------------------------------------------------------------
All ``Plutus`` end-to-end testing requirements will be covered by ``plutus-e2e-tests``.
In summary, with access to the Haskell and Plutus interfaces and reduced friction from using a
single programming language we are likely improve test coverage at this level.
For example, builtin functions and error scenarios.

Although we will be building out the end-to-end test coverage, it is more efficient to have fewer
and broader test scenarios at this level and a greater number of tests at the lower unit and
integration levels for stressing particular features and covering negative scenarios and edge cases.

Examples
~~~~~~~~

* Any Plutus Core builtin function.
  These may already be tested extensively in the lower unit/property/integration levels but there's
  value in having some coverage at the end-to-end level too.

* Use cases that go beyond testing features in isolation.
  Bringing together various functionality
  helps demonstrate the capability of more realistic Plutus applications.
  
* Functionality introduced by a new Plutus version.
  This could mean that ``ScriptContext`` changes to accommodate an extended transaction body.

* Functionality of old Plutus versions tested with each supporting script version.
  Tests for old Plutus version functionality will also be run using the newer script versions.

Argument
--------

.. _`argument 1`:

1. The primary aim is to satisfy all of ``Plutus`` (core) end-to-end testing requirements,
   although, this is an opportunity to also get coverage of other packages being developed such as
   ``cardano-testnet``, ``cardano-ledger-api`` and ``cardano-node-client``.
   We can configure external packages and their versions using CHaPs so there is no need for
   ``plutus-e2e-tests`` to have its own repo. It will initially be be a package in ``plutus-apps``.

.. _`argument 2`:

2. There are a few options for configuring and starting a private testnet (see `local testnet`_
   notes section).
   We will use ``cardano-testnet`` to enable dynamic configuration in Haskell, which makes it easier
   to design tests that can also run in the emulated environment.
   Also, this is the approach officially supported by the node team.

.. _`argument 3`:

3. The plan is to start building tests with ``cardano-api`` because neither ``cardano-ledger-api``
   or ``cardano-node-client`` are at the required stage of development. This allows us to immediately
   proceed with building out the framework and test suite.

.. _`argument 4`:

4. The ambition of ``cardano-ledger-api`` is to be the go-to api for building transactions for
   application developers.
   The UX and overall quality of this component will benefit from being included in these end-to-end
   tests because of the high-level perspective applied during its design and development.
   When ready we will begin incorporating it as a replacement for ``cardano-api``.

.. _`argument 5`:

5. ``cardano-node-client`` will eventually replace ``cardano-api`` as a interface with consensus.
   As the expected means to submit and query for application developers, it is a vital we include it
   under test in ``plutus-e2e-tests``.
   When ready we will begin incorporating it as a replacement for ``cardano-api``.

.. _`argument 6`:

6. Although ``cardano-ledger-api`` and ``cardano-node-client`` are under test it isn't feasible
   to expect thorough coverage of all ledger and node functionality, such as staking and update
   proposals, because the primary focus is to satisfy end-to-end testing requirements for ``Plutus``.
   Fortunately, much of that functionality is already being covered by ``cardano-node-tests``.

.. _`argument 7`:

7. Initially, a few tests will be created without depending on `Plutus-apps
   <https://github.com/input-output-hk/Plutus-apps/>`_.
   This entails building transactions with ``cardano-ledger-api`` and waiting for on-chain events
   using ``cardano-node-client`` without use of the Contract api or the constraints library.
   This approach allows us to build specific transactions, which is especially useful for testing
   edge-cases and error scenarios that off-chain tooling may prohibit.
   However, this approach will require more boilerplate code and this could negatively impact
   readability of the tests.
   Having assessed this approach, we may then decide to depend on ``Plutus-apps`` for the
   Contract api, which would give a uniform interface for off-chain code such as different node
   backends (private and public testnets, and emulator) and chain-indexer queries
   (``cardano-node-client`` or Marconi in future).
   It should also reduce the amount of boilerplate code and provide additional features such as
   trace logging.

.. _`argument 8`:

8. There's value continuing to test ``cardano-cli`` with Plutus transactions for specific cli
   flags and the cli's error handling with script evaluation.
   Some examples of tests that should be covered:

   * Cli flags that require use of Plutus scripts E.g. tx-out-reference-script-file or 
     calculate-plutus-script-cost
   * Cli behaviour when script evaluation passes. This could be displaying expected fee correctly.
   * Cli behaviour when script evaluation fails. This can be how different types errors are formatted. 

9. At some point, we may wish to incorporate the `cardano-node-emulator
   <https://github.com/input-output-hk/Plutus-apps/pull/831>`_ as an alternative to ``cardano-testnet``.
   This would enable us to run property based tests due to the node being much faster without consensus.
   With CHaP, ``cardano-node-emulator`` would be released as a separate component, so no need to depend
   on ``Plutus-apps``.

10. We reserve the option of including additional packages to test from ``Plutus-apps`` at a later stage.

Pros of building and maintaining our own test framework
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Plutus tools team will have full ownership of the end-to-end test environment and its priorities
  for ``Plutus``.
* Plutus scripts can be defined alongside the tests. ``cardano-node-tests`` requires
  pre-compiled scripts.
* Tests will demonstrate how these Haskell packages can be used together to guide Plutus application
  development using the node apis. Particularly useful for less experienced Haskell developers.  
* Possible to define tests once and run at different levels. For example, on private or public
  testnets and with ``cardano-node-emulator`` emulated node.
* Benefits from use of all ``Plutus`` apis. For example, using PlutusTx to produce scripts using a
  typed interface, and optionally the Contract monad from ``Plutus-apps``.
* Have the opportunity to add more components under test at a later stage, such as Marconi or a PAB.
* ``cardano-cli`` would not be a dependency for Plutus test coverage so no risk of being blocked by its
  stage of development.
* Less dependence on repetitive manual approach for regression testing because tests can be planned
  and implemented in parallel with feature implementation and integration.
* Plutus team can implement and review majority of tests in Haskell rather than Python, which is
  likely to be the team's preference.
  Also won't need to review as many tests in ``cardano-node-tests``.
* Less friction caused by cross-team: planning, dependencies and expectations. Plutus team won't
  need to wait for node test team to implement the tests. It's likely that other node/cli features
  will often be prioritised.
* This approach will improve our high-level perspective of each component and help guide
  UX improvements.
* Now that some ``plutus`` tests exist in ``cardano-node-tests`` the process for adding new tests
  will be relatively straightforward, for some it's mostly a copy/paste job.
  This means less work to support some duplicate tests in both frameworks.
* Node team are not pressured to focus on Plutus scenarios, they retain control of their priorities.

Cons of building and maintaining our own test framework
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* ``cardano-node-tests`` is well established and already has useful features such as: running tests in
  different eras, transitioning between eras, reporting, and measuring deviations in script cost.
* It could be quicker for us to get going to reusing the bash scripts ``cardano-node-tests`` have.
  See `local testnet`_ notes section for other examples of spinning up a local testnet.
* We could continue getting ``plutus`` end-to-end test coverage without the need to build our own
  framework because the node test team will continue to maintain theirs regardless.
* Plutus team will still be required to support the node test team with defining and reviewing a
  subset of Plutus tests in ``cardano-node-tests``.
* Node test team may grow, less delays in getting Plutus tests implemented by a Python developer.
* The tests using ``cardano-cli`` already provide some assurance that downstream components are
  working correctly, so there will be some duplication of test coverage by having an additional
  framework.

Additional Considerations
-------------------------
* Business stakeholders will want to see test results to think about producing and storing a report.
  It would be to open source this along with the tests, like ``cardano-node-tests`` have done.
* At first, tests will be run on a private testnet but we must consider how these tests can also be
  run on a public testnet. For example, initial wallet balances and utxos will need to be handled
  dynamically because we'd only have control over these in the private testnet.
* Seeing as ``cardano-ledger-api`` and ``cardano-node-client`` are still in early stages of production
  it would make sense not to block creation of ``plutus-e2e-tests``. We can begin using ``cardano-api``
  and switch over when ready.
* End-to-end tests can be slow to execute and as the suite grows we may want to run a subset at more
  frequent intervals. For example, we run tests for the latest Plutus version nightly but older
  tests/versions are run weekly, or for tags/release only.

Alternatives
------------

Instead of creating a new repository it is possible the end-to-end tests could live in ``Plutus-apps``.
Although, because the components under test span other repositories it would be restrictive and
additional work at the time when dependencies are updated in ``Plutus-apps``, see `argument 1`_.

We could use bash scripts to spin up a local testnet, which is the approach teams such as Djed and
Hydra took.
Although, the decision is to use ``cardano-testnet``, see argument `argument 2`_.

Notes
-----

This ADR document should be moved out of ``Plutus-apps``` and into the new end-to-end test repository
once created.

Benchmarking hasn't been covered above because we already have a team dedicated to testing cardano-node
performance that includes some Plutus scripts. It is an automated approach using ``cardano-cli``.

.. _`local testnet`:

Other places spinning up a local testnet
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* https://github.com/woofpool/cardano-private-testnet-setup
* https://github.com/input-output-hk/mithril/mithril-test-lab
* https://github.com/input-output-hk/hydra/hydra-cluster
* https://github.com/input-output-hk/cardano-node/tree/master/scripts/byron-to-alonzo
* https://github.com/input-output-hk/cardano-js-sdk/tree/master/packages/e2e/local-network
* https://github.com/input-output-hk/cardano-wallet/blob/master/lib/wallet/exe/local-cluster.hs
* https://github.com/mlabs-haskell/plutip
