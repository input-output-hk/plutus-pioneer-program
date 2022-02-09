# Plutus Pioneer Program

## Lectures

### [Lecture #1](https://www.youtube.com/playlist?list=PLNEK_Ejlx3x2nLM4fAck2JS6KhFQlXq2N)

 - [Part 1 - Welcome and Introduction](https://youtu.be/X80uNXenWF4)
 - [Part 2 - The EUTxO-Model](https://youtu.be/bfofA4MM0QE)
 - [Part 3 - Building the Example Code](https://youtu.be/zPaDp4R9X7o)
 - [Part 4 - Auction Contract in the EUTxO-Model](https://youtu.be/Bj6bqRGT1L0)
 - [Part 5 - Auction Contract on the Playground](https://youtu.be/K61Si6iQ-Js)
 - [Part 6 - Homework](https://youtu.be/tfanOE2ARho)

### [Lecture #2](https://www.youtube.com/playlist?list=PLNEK_Ejlx3x0mhPmOjPSHZPtTFpfJo3Nd)

 - [Part 1 - Triggering Change](https://youtu.be/BEr7lcCPjnA)
 - [Part 2 - Low Level, Untyped Validation Scripts](https://youtu.be/xgnmMl-eIIM)
 - [Part 3 - High Level, Typed Validation Scripts](https://youtu.be/HoB_PqeZPNc)
 - [Part 4 - Summary](https://youtu.be/V5P2gKHos48)
 - [Part 5 - Homework](https://youtu.be/_r-EpXzQGKo)

### [Lecture #3](https://www.youtube.com/playlist?list=PLNEK_Ejlx3x2zxcfoVGARFExzOHwXFCCL)

 - [Part 1 - Configuring Playground Time Out](https://youtu.be/sLMhsqiWeGU)
 - [Part 2 - Script Contexts](https://youtu.be/B66xLrGXwmw)
 - [Part 3 - Handling Time](https://youtu.be/mf06ll-4j2w)
 - [Part 4 - A Vesting Example](https://youtu.be/ae7U_yKIQ0Y)
 - [Part 5 - Parameterized Contracts](https://youtu.be/XqFILXV_ACM)
 - [Part 6 - Deploying to the Cardano Testnet](https://youtu.be/ABtffZPoUqU)
 - [Part 7 - Homework](https://youtu.be/GGUT2O_0urQ)
 - [Part 8 - Summary](https://youtu.be/uyaPtayBRb8)

### [Lecture #4](https://www.youtube.com/playlist?list=PLNEK_Ejlx3x230-g-U02issX5BiWAgmSi)

 - [Part 1 - Introduction](https://youtu.be/gxMW9uXTEj4)
 - [Part 2 - Monads](https://youtu.be/f2w-MB3X4a0)
 - [Part 3 - The EmulatorTrace Monad](https://youtu.be/qoUfgaHs1CI)
 - [Part 4 - The Contract Monad](https://youtu.be/yKX5Ce8Y0VQ)
 - [Part 5 - Homework & Summary](https://youtu.be/sxRLzR0jdiY)

### [Lecture #5](https://www.youtube.com/playlist?list=PLNEK_Ejlx3x0G8V8CDBnRDZ86POVsrfzw)

 - [Part 1 - Start](https://youtu.be/mGPqi9m0EPw)
 - [Part 2 - Values](https://youtu.be/4iNTgjovMRg)
 - [Part 3 - A Simple Minting Policy](https://youtu.be/DBUdFsZpW7A)
 - [Part 4 - A More Realistic Minting Policy](https://youtu.be/4SROikF8JwE)
 - [Part 5 - NFT's](https://youtu.be/2lKN0ZL_EQU)
 - [Part 6 - Homework](https://youtu.be/j7yT2OqGY6U)

## Code Examples

 - Lecture #1: [English Auction](code/week01)
 - Lecture #2: [Simple Validation](code/week02)
 - Lecture #3: [Script Contexts & Parameterized Contracts](code/week03)
 - Lecture #4: [Monads, Traces & Contracts](code/week04)
 - Lecture #5: [Native Tokens](code/week05)

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

- Week #2

  - Fix and complete the code in the [Homework1](code/week02/src/Week02/Homework1.hs) module.
  - Fix and complete the code in the [Homework2](code/week02/src/Week02/Homework2.hs) module.

- Week #3

  - Fix and complete the code in the [Homework1](code/week03/src/Week03/Homework1.hs) module.
  - Fix and complete the code in the [Homework2](code/week03/src/Week03/Homework2.hs) module.

- Week #4

  - Implement function `payTrace` in the [Homework](code/week04/src/Week04/Homework.hs) module.
  - Handle exceptions thrown by `submitTx` in function `payContract` in the same module.

- Week #5

  - Implement a Mary-era-style minting policy in the [Homework1](code/week05/src/Week05/Homework1.hs) module.
  - Fix the token name to the empty ByteString in the NFT contract in the [Homework2](code/week05/src/Week05/Homework2.hs) module.

## Some Plutus Modules

  - `Ledger.Scripts`, contains functions related to untyped Plutus scripts.
  - `Ledger.Typed.Scripts`, contains functions related to typed Plutus scripts.
  - `Plutus.V1.Ledger.Ada`, contains definitions and functions related to _ADA_-values.
  - `Plutus.V1.Ledger.Interval`, contains the definition of intervals and functions for working with them.
  - `Plutus.V1.Ledger.Value`, contains definitions and functions related to _values_.
  - `Plutus.V1.Ledger.Time`, contains time-related types and functions.
  - `PlutusTx`, contains important types like `Data` and `BuiltinData`.
  - `PlutusTx.IsData.Class`, contains the `ToData` and `FromData` classes and related functions.
  - `Wallet.Emulator`, contains types and functions for dealing with wallets.

## Additional Resources

- [The Plutus repository](https://github.com/input-output-hk/plutus)
- [The Plutus-Apps repository](https://github.com/input-output-hk/plutus-apps)
- Learn You a Haskell for Great Good: [original](http://learnyouahaskell.com/),
  [remastered](https://hansruec.github.io/learn-you-a-haskell-remastered/01-first-things-first.html) and
  [interactive notebook](https://hub.gke2.mybinder.org/user/jamesdbrock-lea-askell-notebook-24dgdx7w/lab/tree/learn_you_a_haskell/00-preface.ipynb)
- [Haskell & Cryptocurrencies course Mongolia](https://www.youtube.com/playlist?list=PLJ3w5xyG4JWmBVIigNBytJhvSSfZZzfTm)
