# Introduction

The `plutus-simple-model` is a unit test library for Plutus with estimation of resource usage.
The library defines simple mock model of Blockchain to unit test plutus contracts 
and estimate usage of resources. What are the benefits for this framework comparing
to other test frameworks for Plutus? It's:

* easy to use
* easy to think about
* it has minimal dependency set. It depends only on plutus and cardano-ledger.
    Ain't no plutus-apps or cardano-node or ouroboros
* works for both Plutus V1 and Plutus V2
* support for Alonzo and Babbage eras
* blazing fast
* can estimate usage of resources with real functions used on Cardano node.
* pure
* good for unit testing of on-chain code
* being fast and pure it's also suitable for property based testing

Examples of how to use the library can be found in [test suite](https://github.com/mlabs-haskell/plutus-simple-model/tree/main/test) (test dir in the repo).



