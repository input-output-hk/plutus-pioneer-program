.. _common_pab_api:

ADR 6: Common Contract API
==========================

Date: 2022-07-12

Authors
-------

Gergely Szabo <gergely@mlabs.city>

koslambrou <konstantinos.lambrou@iohk.io>

Status
------

Proposed

Context
-------

There are multiple implementations of a Plutus Application Backend (PAB) external of IO Global, and also other tools related to Plutus smart contracts.
Some of them are using the same contract interface as the official implementation, but some of them use a different interface.
However, as the ecosystem evolves, it would be beneficial to create a well defined standard, that other off-chain tools can use as a reference, or as an interface to implement.

Currently, as we are getting close to the Vasil hardfork, testing tools and Plutus Application backend tools are at a hurry to update their dependencies and get to a Vasil compliant/compatible state.
However, tools that are depending on `plutus-apps` are blocked by the PAB development.
This initiative was born out of this context, but could solve other problems as well.

The Contract API (defined in `plutus-apps/plutus-contract`) is using the `freer-simple` effect system to define all the contract effects.
This already allows us to separate the interface from the implementation, and to have multiple implementations/interpreters for one interface.
Currently, there are two implementations for the Contract API:

* one for the plutus-apps emulator (inside `plutus-apps/plutus-contract`)
* one for plutus-apps' Plutus Application Backend (inside `plutus-apps/plutus-pab`)

Therefore, we can leverage this separatation of interface and implementation in order to move the interface out of `plutus-apps`.

Decision
--------

* We will split the `plutus-apps/plutus-contract` package into two parts: the Contract API (`plutus-contract`) and the emulator (`plutus-contract-emulator`).

* We will create effects for the constraints-based transaction builder library (`plutus-apps/plutus-ledger-constraints`) in the Contract API.
  Currently, the interface and the implementation in the transaction builder library are tightly coupled.
  Therefore, we need to decouple them.

* We will create a separate repository with the contract effects and types (the splitted `plutus-contract`).
  By moving the Contract API out of the plutus-apps monorepository, any tool could update to newer version to their discretion.
  Without many dependencies, many tools could utilize the Contract API without having to depend on the whole plutus-apps monorepo.

* We (the Plutus Tools at IO Global) will continue to be the main maintainers of this new repository.
  However, a new ADR will need to be created if we ever decide to make this a community driven project.

* TODO: What about governance? How do we decide which interface changes are accepted? ADRs? Who ultimately accepts and rejects them?

Argument
--------

We speed up the development of off-chain tools, by loosening up some of tightly coupled dependencies, so these external projects can move more freely.
This would also mean that the cost of the interface update would be reduced, so we could see more features added to the standard, and the PAB API following the capabilities of Cardano more closely.
As an added benefit, community involvement with the API could also greatly improve.

A standard API for all Plutus contacts would help keeping the ecosystem on the same track with their implementation.
As more and more off-chain tools implement the same contract interface in the future, it will be relatively easy to switch between different Plutus Application Backend implementations, or to use multiple of these tools at the same time without a need for serious code rewrites.

The implementation of the Contract API interface would track a specific version of the Contract API interface.
We would then need to regularly update the implementation given any interface changes.

Implications
------------

* We will need to decide if we should make this a community driven project.
  If so, we will also need to make a decision about governance.
  How do we decide which interface changes are accepted?
  Do we use ADRs?
  Who ultimately accepts and rejects them?

Notes
-----

This ADR has been discussed here: `#586 <https://github.com/input-output-hk/plutus-apps/pull/586>`_.
