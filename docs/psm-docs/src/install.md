# Install

## With Niv

To add library to your project add it with niv:

```
niv add mlabs-haskell/plutus-simple-model -r <library-commit-hash>
```

And add it to `cabal.project`:

```
-- library for unit tests of Plutus scripts                                                                                       
source-repository-package
   type: git                                                                                                                         
   location: https://github.com/mlabs-haskell/plutus-simple-model
   tag: <same-library-commit-hash-as-for-niv>                                                                              
```

## With Flakes

To add library with flakes we need to add it to the list of inputs (in the flake.nix of your project):
```
TODO: implement simple scaffold for PSM as example
```

