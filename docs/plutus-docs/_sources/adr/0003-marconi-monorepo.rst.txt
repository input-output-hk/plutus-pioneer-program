.. _marconi_monorepo:

ADR 3: Move Marconi into a separate repository
==============================================

Date: 2022-06-08

Authors
---------

Lorenzo Calegari <lorenzo.calegari@iohk.io>

Status
------

Draft

Context
-------

Marconi is a Haskell executable and library that lives in `plutus-chain-index`.

It is desirable to move it into a separate repository for the following reasons:

* Better visibility and easier to discover
* It wants to update the version of its `cardano-api` dependency independently of the version used by `plutus-apps`
* It is a farily independent component, therefore it warrants its own repository

However, creating a separate repository would be rather costly.
It would involve a great deal of duplication, due to the way our current
nix code is structured, not to mention the added complexity and overhead
inherent in maintaining a separate codebase.

Decision
--------

* We will put Marconi in a separate Github repository
* Until we resolve the issues with creating a separate Github repository (see Context), we will keep Marconi as a separate project in `plutus-apps`

Implications
------------

* A nix flake will be added in `plutus-apps` so that users will be able
  to obtain the Marconi executable trivially
* The possibility to specify a separate version of `cardano-api` just for
  Marconi, **while staying in plutus-apps**, will be explored
* As a very low priority task, a new repository *will* be created for Marconi,
  which will use `std` from the start
  (see :ref:`Repository Standardization <repository_standardization>`)

Related Decisions
-----------------

:ref:`Repository Standardization <repository_standardization>`
