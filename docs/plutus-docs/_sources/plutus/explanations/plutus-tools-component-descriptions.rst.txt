.. _plutus_tools_component_descriptions:

Plutus tools in development 
=====================================

The Plutus tools are currently in development. Early iterations of the tools have undergone testing as part of our research into their performance and features. Based on test results and feedback, our team has entered a new cycle of development to address certain design aspects and to place a greater focus on selected features and capabilities. 

Logical components
----------------------------------------------------

Plutus tools consists of libraries, executables and logical components within Haskell packages for external developers to use. 
A single Haskell package may contain multiple logical components. 

The tools are located in the `plutus-apps <https://github.com/input-output-hk/plutus-apps>`_ repository. 

For each tool or logical component shown below, we have indicated its specific location within plutus-apps and provided a brief description. 

.. figure:: ./plutus-tools-ecosystem.png

    Illustration of the Plutus Tools Ecosystem showing logical components and some indication of their dependencies and relationships. (`image source <https://www.plantuml.com/plantuml/uml/tLRDRYCt3BxxATXScW2nS-cffqsyQJT0iWqI1q5F0Jr3nq8qqa3a-0T5txsIHhRbHdRhGoy5vY3-y1C_YfJc2miwwHez-46PIdSrmLoavP-lhXmkH-zAvjsqOtALDK96HfLjhbgD9iGCMMgjfbVZduQFGVNoS7_L80ivhteRswQr9iIq0Vz7w8mFRhm2P4umirhRfJBle9KXG5F3dbavzYiB7HDeMw7MQu-npVBp1eFxwzz_UvcylTvDlhtTt_vw-_th-SLW84eqmLTgjLttwDPg7dYivpKBDM-tApclqDLex7TRqoMS9Mbel7ZcvQVEC22CUpK4zGYUnJfYcFhSFXYiWLhnHwI1WHW3jgrM5OgoXWCHhLZ8X5MCgUQe3D22Z31mLyq16PwirZFhbdwdooXCiVe84aVa9-AkeL-SRKPddjfd0cFhQ5iMzgblSWu6s5a2gxk-aWmvtSkjfAb-9tS9jdDsQftbeI5PMjTpR7Kd7PUJU4MjIVmEhpUtgNpnI3UvBACT0ZKoTSxandQcHhS-x-4drkf0uZ5A44MWodBqFhk0FmRWIHb1PSYbeEVAgLPUHEMXlHA2DdB_Xln1MeDMK-Vt2wfWKxBUg61c7Sn9jw53mm0mB9qIWeJ84P4zCHH0H_5SANZMOCqmdgwdLaZmFXPsbril7yXgD2g3Z0F-dQbfjGEAEeZA0sZNkvQC_5JmJGHddOOQ1M7cR9EVLzwkaQtYd6pKKapGOINmO0tMVq-wmN21A9_2Gx_whnBq7m9Ng76g2PQuFdO6NilYKFMZqbjItm-hdUr0hfEvOgXwO0Q4B68unL2QwyUxlc8LccsznC8x6N8ej6qKMA1MTQBTcIeiuiGNmwUTqdxXq30cr-aQMcSNgWwrmdZLW9tbAOtWLMaTO6fg7LKe_TdDS65Ty4tqPPLJBrTliYfMJbR_lweL-MzwFRQwvxJV-t2-aGELYqETdbyqyMe9IQH9pf-E4Tn9IfEuC6gWZ92ROKKvKdIqircgL8ikKiF23lj3tSErZeJgK20qG4sdKJos_n3Qpm3f9eVpn-kRY-CbkqrOdwksiHpi6zg8ksg4vCn2EMl2onxufsHJ1RARuNXQpu923rZDlIrFBUeX6S9n7iZkFYxtsscktkT3JGlIqDYQT7uyvOFe-p-U2Erl7M8Rr8gRupmcn07-8UoNyqXwY8OJdveNfZ4oPQ-frq1G0QsmQJNnYOYFE0rWrpTmdX3pU_-cxwQBqq6VLUgXXBwLJuJofNT20QKqBUGfez1HJ0_wHBhqHtBeHwrZubrWUY__hVl-ZqLXrw9_uH7C3GUY9gjJ8_9_bUFkfTlrCqQAEXvK9bZSiN0v5K7BiSKRvFJuV79FYC-ciyob69P6SEz_lLpNu7SgSZw76KhHEutQF-_nv-0BcRfhz5y0>`_)

1. Marconi
~~~~~~~~~~~~~

+--------------+--------------------------------------------+
| Marconi                                                   |
+==============+============================================+
| Location     | ``plutus-apps/marconi``                    |
+--------------+--------------------------------------------+

`Marconi <https://github.com/input-output-hk/plutus-apps/tree/main/marconi>`_ is a library for indexing data from the Cardano blockchain. 
`Marconi` is faster and more flexible than `chain-index` and will eventually replace it. 
`Marconi` is currently in an early alpha version. It has several indexers. 
While much of the architecture is settled, some aspects are in the design stage. 

2. Plutus use case examples
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+--------------+--------------------------------------------+
| Plutus use case examples                                  |
+==============+============================================+
| Location     | ``plutus-apps/plutus-use-cases``           |
+--------------+--------------------------------------------+

`Plutus use case examples <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-use-cases>`_ contains hand-written examples for the use cases we currently have. 
The primary examples are: 

   * Auction, 
   * Crowdfunding, 
   * Game, 
   * GameStateMachine, and 
   * Vesting. 

For each Plutus application use case, we provide test scenarios (or test cases) with and without the :hsmod:`Plutus.Contract.Test.ContractModel`.

The examples are for testing and educational purposes. 
While they work in the `plutus-contract emulator`, they are not guaranteed to work on the actual Cardano network, primarily because the size of the produced Plutus scripts are too big to fit in a transaction given current protocol parameters. 

3. PAB (Plutus application backend)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+--------------+--------------------------------------------+
| PAB                                                       |
+==============+============================================+
| Location     | ``plutus-apps/plutus-pab``                 |
+--------------+--------------------------------------------+

`PAB <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-pab>`_ 
is a web server library for managing the state of Plutus contract instances. 
The PAB executes the off-chain component of Plutus applications. It manages 
application requests to the wallet backend, the Cardano node and the chain-index. 
PAB stores the application state and offers an HTTP REST API for managing application 
instances. 

PAB wraps the contracts built with plutus-contract. It is the central point of 
contact, integrating many Cardano components. 

4. Contract monad emulator
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+--------------+--------------------------------------------+
| Contract monad emulator                                   |
+==============+============================================+
| Location     | ``plutus-apps/plutus-contract``            |
+--------------+--------------------------------------------+

`Contract monad emulator <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-contract>`_ is a library that provides an environment for emulating the blockchain. 
The environment provides a way for writing traces for the contract which are sequences of actions by simulated wallets that use the contract. 
The component is highly dependent on the Contract API (Contract monad). 

5. Plutus contract model testing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+--------------+--------------------------------------------+
| Plutus contract model testing                             |
+==============+============================================+
| Location     | ``plutus-apps/plutus-contract``            |
+--------------+--------------------------------------------+

`Plutus contract model testing <https://github.com/input-output-hk/plutus-apps/blob/main/plutus-contract/src/Plutus/Contract/Test/ContractModel.hs>`_ is used for testing prototype Plutus contracts with *contract models*, using the framework provided by :hsmod:`Plutus.Contract.Test.ContractModel`. 
This framework generates and runs tests on the Plutus emulator, where each test may involve a number of emulated wallets, each running a collection of Plutus contracts, all submitting transactions to an emulated blockchain. 
Once you have defined a suitable model, then QuickCheck can generate and run many thousands of scenarios, taking the application through a wide variety of states, and checking that it behaves correctly in each one. 

   See the following tutorials: 
   
   * :doc:`Property-based testing of Plutus contracts <../tutorials/contract-testing>` 
   * :doc:`Testing Plutus Contracts with Contract Models <../tutorials/contract-models>` 

6. Plutus contract state machine
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+--------------+--------------------------------------------+
| Plutus contract state machine                             |
+==============+============================================+
| Location     | ``plutus-apps/plutus-contract``            |
+--------------+--------------------------------------------+

`Plutus contract state machine <https://github.com/input-output-hk/plutus-apps/blob/main/plutus-contract/src/Plutus/Contract/StateMachine.hs>`_ is a library that is a useful high-level tool for defining and modeling a Plutus application (smart contract) based on the State Machine formalism. 
It is helpful for writing a reference implementation for testing before creating the production version. 
However, we do not recommend using it in production as the scripts are too big to run on-chain. 

7. Contract API (also known as Contract monad)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+--------------+--------------------------------------------+
| Contract API                                              |
+==============+============================================+
| Location     | ``plutus-apps/plutus-contract``            |
+--------------+--------------------------------------------+

`Contract API <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-contract>`_ is a logical component within the Plutus Contract package, providing an effect system for describing smart contracts that interact with wallets, DApps, a chain indexer and the blockchain. 
It provides the Contract API interface for writing the off-chain part of a Plutus application that is to be interpreted by an emulator or by Plutus application backend (PAB). 

8. Plutus chain index
~~~~~~~~~~~~~~~~~~~~~~~~

+--------------+--------------------------------------------+
| Plutus chain index                                        |
+==============+============================================+
| Location     | ``plutus-apps/plutus-chain-index-core``    |
+--------------+--------------------------------------------+
|              | ``plutus-apps/plutus-chain-index``         |
+--------------+--------------------------------------------+

`Plutus chain index <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-chain-index-core>`_ 
is an application for indexing data from the Cardano blockchain that is used in the Contract Monad. 
The main design goal is to keep the size of the indexed information proportional to the UTXO set. 

9. Plutus ledger constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+--------------+--------------------------------------------+
| Plutus ledger constraints                                 |
+==============+============================================+
| Location     | ``plutus-apps/plutus-ledger-constraints``  |
+--------------+--------------------------------------------+

`Plutus ledger constraints <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-ledger-constraints>`_ contains a constraints-based API that can be used to generate on-chain validation functions and to build transactions by providing a list of constraints. 
The main design goal is to be able to use the same constraints on-chain and off-chain in a Plutus application. 
The off-chain part generates transactions based on types in `Plutus ledger <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-ledger>`_. 

For example:

   * ``checkScriptContext (MustSpendAtLeast 10Ada, MustProduceOutput myOutput, …)``
   * ``mkTx (MustSpendAtLeast 10Ada, MustProduceOutput myOutput, …)``

10. Plutus Tx constraints 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+--------------+--------------------------------------------+
| Plutus Tx constraints                                     |
+==============+============================================+
| Location     | ``plutus-apps/plutus-tx-constraints``      |
+--------------+--------------------------------------------+

`Plutus-tx-constraints <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-tx-constraints>`_ contains a constraints-based API that can be used to generate on-chain validation functions and to build transactions by providing a list of constraints. 
It is intended to support all the functionality from `Plutus ledger constraints <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-ledger-constraints>`_. 
The main design goal is to be able to use the same constraints on-chain and off-chain in a Plutus application. 
The off-chain part generates transactions based on types in `cardano-api <https://input-output-hk.github.io/cardano-node/cardano-api/lib/Cardano-Api.html>`_. 

For example:

   * ``checkScriptContext (MustSpendAtLeast 10Ada, MustProduceOutput myOutput, …)``
   * ``mkTx (MustSpendAtLeast 10Ada, MustProduceOutput myOutput, …)``

11. Plutus ledger
~~~~~~~~~~~~~~~~~~~~

+--------------+--------------------------------------------+
| Plutus ledger                                             |
+==============+============================================+
| Location     | ``plutus-apps/plutus-ledger``              |
+--------------+--------------------------------------------+

`Plutus ledger <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-ledger>`_ is a set of transitional types that simplify the cardano-api types. 
It is intended to be a comprehensive, easy-to-use set of types that replicate the current era of `cardano-api <https://input-output-hk.github.io/cardano-node/cardano-api/lib/Cardano-Api.html>`_. 
It currently considers only the last era. 
Plutus ledger contains data types and functions that complement `cardano-ledger <https://github.com/input-output-hk/cardano-ledger>`_ related to Plutus. 

12. Plutus script utils
~~~~~~~~~~~~~~~~~~~~~~~~~~

+--------------+--------------------------------------------+
| Plutus script utils                                       |
+==============+============================================+
| Location     | ``plutus-apps/plutus-script-utils``        |
+--------------+--------------------------------------------+

`Plutus script utils <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-script-utils>`_ is a utility library for helping users write Plutus scripts that are to be used on-chain. `Plutus script utils` includes a variety of useful functions for on-chain operations in Plutus scripts. 

It provides a number of utilities including: 

   * hashing functions for Datums, Redeemers and Plutus scripts for any Plutus language version. 
   * functionality for wrapping the untyped Plutus script with a typed interface. 
   * utility functions for working with the ScriptContext of a Plutus Script. 
