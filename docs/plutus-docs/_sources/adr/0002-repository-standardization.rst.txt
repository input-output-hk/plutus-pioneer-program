.. _repository_standardization:

ADR 2: Repository Standardization
=======================================

Date: 2022-07-06

Authors
---------

Lorenzo Calegari <lorenzo.calegari@iohk.io>

Status
------

Draft

Context
-------

IOG is undertaking a company-wide effort to restructure and standardize its 
repositories, favoring mono-repos and enforcing shared GitOps and DevOps 
processes. Parallel to this, a new CI infrastructure is being developed.

Examples of this are:

* `input-output-hk/cardano-world <https://github.com/input-output-hk/cardano-world>`_
* `input-output-hk/ci-world <https://github.com/input-output-hk/ci-world>`_
* `input-output-hk/atala-world <https://github.com/input-output-hk/atala-world>`_
  
This initiative appears to be championed by the SRE team who are the creators of 
`divnix/std <https://github.com/divnix/std>`_. Indeed `std` is at the heart of 
the standardization dream.

Decision
--------

* Standardization of the repositories has been deemed a worthwhile endeavour, 
  though of very low priority. 
* Phase 1 of the standardization process will be carried out in parallel with 
  :ref:`Move Marconi to a separate repository <marconi_monorepo>`.
  A separate repository will be created for Marconi, and from the very beginning 
  it will use `std`. This way the benefits, limitations and integration costs of 
  `std` can be experienced and measured, and an informed, definitive decision on 
  standardizing `plutus-core` and `plutus-apps` themselves can be made. 

Argument
------------

In short, `std` aims to answer the one critical question that pops in the mind 
of newcomers and veterans alike: 

*What can I do with this repository?*

In practice, `std` is flake-based nix library code that provides a 
strongly-but-sensibly-opinionated top-level interface for structuring all your 
nix code.

This is wonderful news for the owner of the repository's nix code, but what
about every other stakeholder? Especially developers who don't care/know about 
nix? 

Contributors of a standardized codebase will be gifted with a TUI to discover 
and interact with the repository, which is probably something that is long 
overdue as an industry-level best-practice.

Who wouldn't want to clone a repository, type `std` and be presented with a TUI 
that gives you an interactive tour of the repository's artifacts, together with 
a list of all possible DevOps and GitOps actions (build, test, develop, run, 
deploy, benchmark, publish, package, monitor, ...) in addition to any other 
action that you may define. 

And for power users and automators, there is an equivalent CLI to the TUI. 
This makes `README` files obsolete to an extent. 
A TUI/CLI combo represents the best conceivable solution in terms of user 
experience (only a GUI could top that perhaps).

In conclusion, the advantages of standardizing the repositories are:

* Enforce a shared mental model for internal and external teams to effortlessly 
  reason about the codebase.
* Provide a TUI/CLI to more easily discover, interact with, and contribute to 
  the repository, with the goal to provide a superior user experience to all 
  stakeholders.
* Refactor all existing Nix code into a supposedly far better structure. 
  `std` seems to solve the "import problem" by automatically parsing the 
  directory structure and threading all derivations into a globally accessible 
  top-level scope, drastically reducing the average length of paths in the 
  dependency graph, both at the file level and at the term/variable level. 
  This all translates into cleaner, more maintainable code.

Implications
------------

The plutus repositories now exhibit a large amount of duplicated nix 
(and configuration) code, as a result of the split into `plutus-core`
and `plutus-apps`.

While introducing `std` will not in itself help reduce duplication, the 
refactoring process will involve identifying and isolating shared components 
that can be later packaged and separated into library code. 

The goal is to standardize both repositories, by introducing `std` and 
refactoring all existing nix code accordingly. 

The SRE team has also created several other satellite repositories containing 
reusable nix code to support this process, though it is unclear at this stage 
whether these are relevant to standardizing `plutus-core` and `plutus-apps`.

Such repositories include:

* `nixagii <https://github.com/input-output-hk/nixagii>`_
* `devshell-capsules <https://github.com/input-output-hk/devshell-capsules>`_
* `kladoi <https://github.com/input-output-hk/kladoi>`_
  

The standardization process would follow the `4 Layers SRE Mental Model <https://sre-manual.infra.aws.iohkdev.io/mental-model.html>`_, 
which begins by introducing `std` in Layer 1 (binary packaging).
Layers 2-3-4 (which is mostly DevOps) will be postponed to a later date, once 
the migration to the new CI systems has been officially approved and initiated.  
