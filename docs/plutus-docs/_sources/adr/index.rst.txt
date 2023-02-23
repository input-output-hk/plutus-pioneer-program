Architectural Decision Records
==============================

We document our architectural and design decisions for all of our components.
In order to do that, there is practice called architectural decision records ("ADR"),
that we can integrate into our workflow.
An architectural decision record (ADR) is a document that captures an important architectural decision made along with its context and consequences.

The goals are:

* making decisions transparent to internal/external stakeholders and contributors.

* getting feeback on decisions that we're about to make or have made

* providing external contributors a framework to propose architectural changes

* providing a big picture of all major decisions that were made

The general process for creating an ADR is:

1. cloning the repository

2. creating a new file with the format `<ADR_NUMBER>-<TITLE>.rst` in the directory `doc/adr`

3. adding the ADR in the table of content tree of the Readthedocs

4. committing and pushing to the repository

.. toctree::
   :maxdepth: 1
   :titlesonly:

   0001-record-architecture-decisions
   0002-repository-standardization
   0003-marconi-monorepo
   0004-marconi-initiative
   0005-pab-indexing-solution-integration
   0006-common-contract-api
   0007-support-reference-inputs-in-constraint-library
   0008-support-inline-datums-in-constraint-library
   0009-support-reference-scripts-in-constraint-library
   0010-marconi-indexer-rollbacks
   0011-support-return-and-total-collateral-when-building-transactions
   0012-commit-to-data-types-in-cardano-api
   0013-tx-validity-time-range-fix
   0014-marconi-query-interface
   0015-time-conversion
   0016-cardano-node-emulator
   0017-end-to-end-testing-strategy
