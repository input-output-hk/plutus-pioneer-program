# Plutus Pioneer Program

## Lectures

- [Lecture #1](https://youtu.be/IEn6jUo-0vU)

  - Welcome
  - The (E)UTxO-model
  - Running an example auction contract on a local Playground
  - Homework

- [Lecture #2](https://youtu.be/E5KRk5y9KjQ)

  - Triggering change.
  - Low-level, untyped on-chain validation scripts.
  - High-level, typed on-chain validation scripts.

- [Lecture #3](https://youtu.be/Lk1eIVm_ZTQ)

  - Script context.
  - Time handling.
  - Parameterized contracts.

- [Lecture #4](https://youtu.be/6Reuh0xZDjY)

  - Monads
  - The `EmulatorTrace` monad.
  - The `Contract` monad.

- [Lecture #5](https://youtu.be/6VbhY162GQA)

  - Values.
  - Native tokens & minting policies.
  - NFT's.

- [Lecture #6](https://youtu.be/wY7R-PJn66g)

  - Oracles.
  - Using the PAB.

- [Lecture #7](https://youtu.be/oJupInqvJUI)

  - Commit schemes.
  - State machines.

- [Lecture #8](https://youtu.be/JMRwkMgaBOg)

  - Another state machine example: token sale.
  - Automatic testing using emulator traces.
  - Interlude: optics.
  - Property based testing with `QuickCheck`.
  - Testing Plutus contracts with property based testing.

## Code Examples

- Lecture #1: [English Auction](code/week01)
- Lecture #2: [Simple Validation](code/week02)
- Lecture #3: [Validation Context & Parameterized Contracts](code/week03)
- Lecture #4: [Monads, `EmulatorTrace` & `Contract`](code/week04)
- Lecture #5: [Minting Policies](code/week05)
- Lecture #6: [Oracles](code/week06)
- Lecture #7: [State Machines](code/week07)
- Lecture #8: [Testing](code/week08)

## Exercises

- Week #1

  - Build the [English Auction](code/week01) contract with `cabal build` (you may need to run `cabal update` first).
  - Clone the [The Plutus repository](https://github.com/input-output-hk/plutus), check out the correct commit
    as specified in [cabal.project](code/week01/cabal.project).
  - Set-up IOHK binary caches [How to set up the IOHK binary caches](https://github.com/input-output-hk/plutus#iohk-binary-cache). "If you do not do this, you will end up building GHC, which takes several hours. If you find yourself building GHC, STOP and fix the cache."
  - Enter a `nix-shell`.
  - Go to the `plutus-playground-client` folder.
  - Start the Playground server with `plutus-playground-server`.
  - Start the Playground client (in another `nix-shell`) with `npm run start`.
  - Copy-paste the auction contract into the Playground editor - don't forget to remove the module header!
  - Compile.
  - Simulate various auction scenarios.

- Week #2

  - Fix and complete the code in the [Homework1](code/week02/src/Week02/Homework1.hs) module.
  - Fix and complete the code in the [Homework2](code/week02/src/Week02/Homework2.hs) module.

- Week #3

  - Fix and complete the code in the [Homework1](code/week03/src/Week03/Homework1.hs) module.
  - Fix and complete the code in the [Homework2](code/week03/src/Week03/Homework2.hs) module.

- Week #4

  - Write an appropriate `EmulatorTrace` that uses the `payContract` contract in the [Homework](code/week04/src/Week04/Homework.hs) module.
  - Catch errors in the `payContract` contract in the same module.

- Week #5

  - Add a deadline to the minting policy in the [Homework1](code/week05/src/Week05/Homework1.hs) module.
  - Fix the token name to the empty `ByteString` in the NFT contract in the [Homework2](code/week05/src/Week05/Homework2.hs) module.

- Week #6

  - Get the Oracle demo running and extend it in some way.

- Week #7

  - Implement the game of "Rock, Paper, Scissors" using state machines.

- Week #8

  - Add a new operation `close` to the `TokenSale`-contract that allows the seller to close the contract and
    retrieve all remaining funds (including the NFT).
  - Modify the tests accordingly.

## Solutions

- Week #2

  - [`Homework1`](code/week02/src/Week02/Solution1.hs)
  - [`Homework2`](code/week02/src/Week02/Solution2.hs)

- Week #3

  - [`Homework1`](code/week03/src/Week03/Solution1.hs)
  - [`Homework2`](code/week03/src/Week03/Solution2.hs)

- Week #4

  - [`Homework`](code/week04/src/Week04/Solution.hs)

- Week #5

  - [`Homework1`](code/week05/src/Week05/Solution1.hs)
  - [`Homework2`](code/week05/src/Week05/Solution2.hs)

- Week #7

  - [`RockPaperScissors`](code/week07/src/Week07/RockPaperScissors.hs)
  - [`TestRockPaperScissors`](code/week07/src/Week07/TestRockPaperScissors.hs)

## Some Plutus Modules

- [`Plutus.Contract.StateMachine`](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Contract/StateMachine.hs), contains types and functions for using state machines.
- [`Plutus.Contract.Test`](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Contract/Test.hs), provides various ways to write tests for Plutus contracts.
- [`Plutus.Contract.Test.ContractModel`](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Contract/Test/ContractModel.hs), support for property based testing of Plutus contracts.
- [`Plutus.PAB.Webserver.API`](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/src/Plutus/PAB/Webserver/API.hs), contains the HTTP-interface provided by the PAB.
- [`Plutus.Trace.Emulator`](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Trace/Emulator.hs), contains types and functions related to traces.
- [`Plutus.V1.Ledger.Ada`](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Ada.hs), contains support for the Ada currency.
- [`Plutus.V1.Ledger.Contexts`](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Contexts.hs), contains the definition of the context-related types.
- [`Plutus.V1.Ledger.Interval`](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Interval.hs), contains the definition of and helper functions for the `Interval` type.
- [`Plutus.V1.Ledger.Slot`](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Slot.hs), contains the definition of the `Slot` type.
- [`Plutus.V1.Ledger.Value`](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Value.hs), contains the definition of and helper functions for the `Value` type.
- [`PlutusTx.Data`](https://github.com/input-output-hk/plutus/blob/master/plutus-tx/src/PlutusTx/Data.hs), contains the definition of the `Data` type.
- [`PlutusTx.IsData.Class`](https://github.com/input-output-hk/plutus/blob/master/plutus-tx/src/PlutusTx/IsData/Class.hs), defines the `IsData` class.

## Plutus community playground

- [Week 1 Community Playground(Legacy)](https://playground-week1.plutus-community.com/)
- [Week 2 Community Playground(Legacy)](https://playground-week2.plutus-community.com/)
- [Week 3 Community Playground(Current)](https://playground-week3.plutus-community.com/)
- [Week 4 Community Playground(Current)](https://playground.plutus-community.com/)


## Additional Resources

- [The Plutus repository](https://github.com/input-output-hk/plutus)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- [Haskell & Cryptocurrencies course Mongolia](https://www.youtube.com/playlist?list=PLJ3w5xyG4JWmBVIigNBytJhvSSfZZzfTm)
