.. _what_is_the_pab:

What is the PAB?
================

PAB is short for *Plutus Application Backend*.
The Plutus Application Backend is the client-side runtime for :term:`Plutus apps<contract application>` that are built with the `Plutus Platform <https://plutus.readthedocs.io/en/latest/explanations/platform.html>`_.
It is the PAB's task to deal with requests from running ``Contract`` instances, to forward user input to them, and to notify them of ledger state change events.

.. TODO: Ref. to `Contract` type

The ``plutus-pab`` cabal package in the Plutus repository defines a ``plutus-pab`` Haskell library.
Application developers use this library to build the actual PAB executable, specialised to one or more of their ``Contract`` s.

.. note::
    In an older version of the PAB, each ``Contract`` was compiled to a separate executable, and there was a single PAB that knew about all the locally available excutable contracts.
    This approach is not supported anymore.


Client interface
----------------

The PAB provides an HTTP and websocket interface for interacting with ``Contract`` instances.
All PAB operations, including starting new instances, calling endpoints on instances, and querying instance state, are performed using this API.
Application developers can build their own frontends and server processes that make HTTP calls to the PAB.

Other components
----------------

In addition to the PAB itself, the following components are required.

.. _pab_chain_index:

Chain index
~~~~~~~~~~~

The chain index is a database of data gathered from Cardano transactions.
It uses the Cardano node's chain sync protocol.
Therefore it needs to be co-located with a Cardano node.
The chain index is a read-only component for the PAB.
Multiple instances of the PAB can therefore share a single instance of the chain index.

The expressiveness of queries supported by the chain index lies somewhere between that of the node, which answers queries related to the ledger state, and that of ``db-sync``, which has a full history of all transactions and an expressive database schema for staking and other information.

All chain index queries are served over an HTTP API.

Alonzo node
~~~~~~~~~~~

The PAB subscribes to ledger state updates from the node, using a socket protocol.

Wallet
~~~~~~

A Cardano wallet is required for balancing and signing transactions (and optionnaly submitting transactions).
Balancing means taking a partial transaction and adding inputs and outputs to make the transaction valid.

Take `Marlowe <https://play.marlowe-finance.io/doc/marlowe/tutorials/introducing-marlowe.html>`_ as an example.
When the user first starts a Marlowe contract, funds need to be transferred from one of the user's addresses to the contract address.
This is achieved by sending a partial transaction that has zero inputs and a script output for the Marlowe contract instance to the wallet for balancing.
The wallet adds some of its own inputs to cover the amount that is to be paid into the contract, plus a change output for any excess funds.
When the Marlowe contract has finished, funds are transferred back to the user's wallet using the same mechanism:
The PAB sends another partial transaction, this time with a single script input and no outputs.
The wallet then adds an output at one of its own addresses to receive the funds.

There are multiple ways to setup a wallet:

1. Host a cardano wallet backend instance (WBE) using `cardano-wallet <https://github.com/input-output-hk/cardano-wallet>`_
2. Setup a desktop wallet application (ex. `Daedalus <https://daedaluswallet.io/>`_)
3. Setup a browser wallet application (ex. `Nami <https://namiwallet.io>`_, `Yoroi <https://yoroi-wallet.com>`_, etc.)

These different wallet setups each imply a different use-case of the PAB.

Deployment Scenarios
--------------------

There are two deployment models envisaged for the PAB: Hosted and in-browser.
The hosted variant will be supported at the initial release of the PAB.
The in-browser variant wil be available after the initial release.

Hosted
~~~~~~

In the “Hosted PAB” scenario, the dApp provider / developer hosts an instance of the PAB alongside the :ref:`chain index<pab_chain_index>` and an Alonzo node.
The off-chain code of the Plutus app is run on the dApp provider’s infrastructure.

In the following sections, we illustrate the ways a hosted PAB can be used with the different type of wallets.

WBE (Supported)
^^^^^^^^^^^^^^^

In this wallet scenario, the dApp provider /developer also hosts an instance of the WBE, which handles the wallets for each user.
The WBE handles balancing, signing and submitting transaction requests from the PAB.

.. figure:: ./hosted-pab-wbe.png

    The hosted deployment scenario for the PAB with the WBE

This is currently used for testing purposes and shouldn't be used in a production setting, because we wallets are normally controlled by the users themselves.

A simple demo of this scenario is available here: `<https://github.com/input-output-hk/plutus-apps/tree/main/plutus-pab/test-node>`_.

Desktop wallet (Not yet supported)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In this wallet scenario, the user has setup a desktop wallet (light or full node) such as Daedalus.
Transaction balancing (coin selection) and transaction signing (in short: anything that deals with the user’s money) happens on the user’s machine.
The PAB produces a link (URI) for each partial transaction that needs to be balanced, signed and submitted.
When the user clicks the link, the user's operating system opens the wallet that is registered to handle the link schema.
This scheme is not restricted to Daedalus, or even to full node wallets.
Any wallet that implements a handler for the link schema can be used to balance, sign and submit Plutus transactions.

.. figure:: ./hosted-pab-cardano-wallet.png

    The hosted deployment scenario for the PAB communicating with a desktop wallet.

Browser wallet (In progress)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In this wallet scenario, the user has setup a browser wallet such as Nami or Yoroi.
The PAB updates it's contract instance status endpoint for each partial transaction that needs to be balanced, signed and submitted.
Transaction signing happens on the user's machine.
However, transaction balancing (coin selection) is handled by the PAB as it is not currently possible to balance transaction that contain script inputs in the browser (i.e. browser wallets can't balance transactions until it is possible to execute Plutus script in the browser).
Therefore, browser wallets will need to call a PAB helper endpoint which can balance the transaction using funds from the user's browser wallet.

.. figure:: ./hosted-pab-browser-wallet.png

    The hosted deployment scenario for the PAB communicating with a browser wallet.

In-browser
~~~~~~~~~~

In the “In-browser PAB” scenario, the dApp provider / developer hosts an instance of the :ref:`chain index<pab_chain_index>` and an Alonzo node.
The dApp users work with a browser interface which uses a light version of the PAB.

Similary to the hosted PAB scenario, we illustrate the ways it can be used the different type of wallets.

Desktop wallet (Not yet supported)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. figure:: ./in-browser-pab-cardano-wallet.png

    The in-browser PAB communicating with a desktop wallet.

Browser wallet (Not yet supported)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. figure:: ./in-browser-pab-browser-wallet.png

    The in-browser PAB communicating with a browser wallet.
