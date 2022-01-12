# Plutus Pioneer Program

## Lectures

### Lecture #1

 - [Part 1 - Welcome and Introduction](https://youtu.be/X80uNXenWF4)
 - [Part 2 - The EUTxO-Model](https://youtu.be/bfofA4MM0QE)
 - [Part 3 - Building the Example Code](https://youtu.be/zPaDp4R9X7o)
 - [Part 4 - Auction Contract in the EUTxO-Model](https://youtu.be/Bj6bqRGT1L0)
 - [Part 5 - Auction Contract on the Playground](https://youtu.be/K61Si6iQ-Js)
 - [Part 6 - Homework](https://youtu.be/tfanOE2ARho)

## Code Examples

 - Lecture #1: [English Auction](code/week01)

## Exercises

- Week #1

  - Clone the [The Plutus-Apps repository](https://github.com/input-output-hk/plutus-apps), check out the correct commit
    as specified in [cabal.project](code/week01/cabal.project).
  - Install NixOS cross-referencing the following resources.
     - https://nixos.org/download.html
     - https://docs.plutus-community.com
     - A few resources to understand the what and why regarding NixOS
       - https://nixos.org/manual/nix/stable
       - https://serokell.io/blog/what-is-nix
  - Set-up IOHK binary caches [How to set up the IOHK binary caches](https://github.com/input-output-hk/plutus#iohk-binary-cache). "If you do not do this, you will end up building GHC, which takes several hours. If you find yourself building GHC, *stop* and fix the cache."
  - Enter a `nix-shell`.
  - Build the [English Auction](code/week01) contract with `cabal build` (you may need to run `cabal update` first).
  - Go to the `plutus-playground-client` folder in the `plutus-apps` repository.
  - Start the Playground server with `plutus-playground-server`.
  - Start the Playground client (in another `nix-shell`) with `npm start`.
  - Copy-paste the auction contract into the Playground editor, do not forget to remove the module header.
  - Compile.
  - Simulate various auction scenarios.

## Some Plutus Modules

## Additional Resources

- [The Plutus repository](https://github.com/input-output-hk/plutus)
- [The Plutus-Apps repository](https://github.com/input-output-hk/plutus-apps)
- Learn You a Haskell for Great Good: [original](http://learnyouahaskell.com/),
  [remastered](https://hansruec.github.io/learn-you-a-haskell-remastered/01-first-things-first.html) and
  [interactive notebook](https://hub.gke2.mybinder.org/user/jamesdbrock-lea-askell-notebook-24dgdx7w/lab/tree/learn_you_a_haskell/00-preface.ipynb)
- [Haskell & Cryptocurrencies course Mongolia](https://www.youtube.com/playlist?list=PLJ3w5xyG4JWmBVIigNBytJhvSSfZZzfTm)
