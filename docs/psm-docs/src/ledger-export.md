# Ledger reexports

As plutus-ledger is not available with removing plutus-apps dependency. 
We still provide some useful functionality from it. We have:

* `Plutus.Model.Ada` - for type-safe wrapper for Ada values
* `Plutus.Model.Validator` - for typed validators and minting policies and calculation of hashes for them.

### Plutus onchain goodies

Also library defines some handy functions to use with plutus like `datumOf`, `inlinedDatum`, `forwardTo`.
See the modules `Plutus.Model.Validator.[V1/V2].Plutus`.
It's exported by default with `Plutus.Model.Vn`.


