Plutus tools SDK user guide
===============================

Plutus tools SDK
-------------------------------

The Plutus Tools SDK is a collection of off-chain infrastructure resources built 
for external developers. The `Plutus Tools SDK repository <https://github.com/
input-output-hk/plutus-apps>`_ supports the underlying resources that developers 
need who are writing full applications using Plutus in Haskell, including off-chain 
code. The term “off-chain code” refers to the part of a contract application’s code 
which runs outside of the blockchain. Off-chain code responds to events happening 
on or off the blockchain, usually by producing transactions. 

This user guide is intended for developers who are authoring distributed applications 
("DApps") by using smart contracts on the Cardano blockchain. 

.. note::
   If you are a developer who wants to contribute to the Plutus Tools SDK project, 
   please refer to documentation residing in the `Plutus Tools SDK repository <https://github.com/
   input-output-hk/plutus-apps>`_.

Plutus tools SDK repository
------------------------------------------------

The `Plutus Tools SDK repository <https://github.com/input-output-hk/plutus-apps>`_ 
contains packages such as: 

* the `Plutus Application Backend (PAB) <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-pab-executables>`_, 
  an off-chain application for managing the state of Plutus contract instances;

* the `chain-index <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-chain-index>`_, 
  a lightweight, customizable chain follower application and library for DApp 
  developers who need to index and query the Cardano blockchain; 

* the `Plutus Contract Package <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-contract>`_, 
  a library for writing Plutus contracts and transforming them into executables 
  that run on the application platform; 

* the `Plutus Ledger Constraints Package <https://github.com/input-output-hk/plutus-apps/tree/a5ab40d6b7f28646a69da03d303a39982cc10b68/plutus-ledger-constraints>`_, 
  containing an API to build transactions by providing a list of constraints and 
  for constructing and validating Plutus transactions; 

* the `Trace Emulator <https://github.com/input-output-hk/plutus-apps/blob/main/plutus-contract/src/Plutus/Trace/Emulator.hs>`_, 
  used for testing Plutus contracts on an emulated blockchain; 

* a variety of other Plutus packages. 

Use cases
---------------------------

Please refer to these `use cases <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-use-cases>`_ 
to see examples of Plutus applications. 

Plutus starter template repository
----------------------------------------

See the `Plutus starter template repository <https://github.com/input-output-hk/plutus-starter>`_ 
for a simple starter project using the Plutus Tools SDK. 

Public Plutus libraries documentation
------------------------------------------------

See also the `public Plutus libraries documentation <https://playground.plutus.iohkdev.io/doc/haddock/>`_ 
to access Haddock-generated documentation of all the code, including Plutus Core. 

.. toctree::
   :caption: Explore Plutus
   :maxdepth: 2

   plutus/explanations/index
   plutus/tutorials/index
   plutus/howtos/index
   plutus/troubleshooting

.. toctree::
   :caption: Architectural decision records
   :maxdepth: 1

   adr/index

.. toctree::
   :caption: Reference
   :maxdepth: 2

   reference/index
