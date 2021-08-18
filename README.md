# Plutus Pioneer Program

## Lectures

- [Lecture #1](https://youtu.be/_zr3W8cgzIQ)

  - Welcome.
  - The (E)UTxO-model.
  - Building the example code.
  - An auction contract in the EUTxO-model.
  - A brief look at the auction code.
  - Running an example auction contract on a local Playground.
  - Homework.

- [Lecture #2](https://youtu.be/sN3BIa3GAOc)

  - Triggering change.
  - Low-level, untyped on-chain validation scripts.
  - High-level, typed on-chain validation scripts.

- [Lecture #3](https://youtu.be/6_rfCCY9_gY)

  - Script contexts.
  - Time handling.
  - Parameterized contracts.

- [Lecture #4](https://youtu.be/g4lvA14I-Jg)

  - Monads.
  - The `EmulatorTrace`-monad.
  - The `Contract`-monad.

- [Lecture #5](https://youtu.be/SsaVjSsPPcg)

  - Values.
  - Native Tokens.
  - NFT's.

- [Lecture #6](https://youtu.be/24SHPHEc3zo)

  - Oracles.
  - Using the PAB.

- [Lecture #7](https://youtu.be/uwZ903Zd0DU)

  - Commit schemes.
  - State machines.

- [Lecture #8](https://youtu.be/zW3D2iM5uVg)

  - Another state machine example: token sale.
  - Automatic testing using emulator traces.
  - Interlude: optics.
  - Property based testing with QuickCheck.
  - Testing Plutus contracts with property based testing.

- [Lecture #9](https://youtu.be/H1WPL01qWCc)

  - Marlowe overview ([slides](Marlowe_Plutus_Pioneers_June_2021.pdf)).
  - Marlowe in Plutus.
  - Marlowe Playground demo.

- [Lecture #10](https://youtu.be/CPfcyDaDtt8)

  - Uniswap overview.
  - Uniswap implementation in Plutus.
  - Deploying Uniswap with the PAB.
  - Demo.
  - Using `curl` to interact with the PAB.

## Code Examples

- Lecture #1:  [English Auction](code/week01)
- Lecture #2:  [Simple validation](code/week02)
- Lecture #3:  [Script Context & Parameterized Contracts](code/week03)
- Lecture #4:  [Monad, Traces & Contracts](code/week04)
- Lecture #5:  [Native Tokens](code/week05)
- Lecture #6:  [Oracles](code/week06)
- Lecture #7:  [State Machines](code/week07)
- Lecture #8:  [Testing](code/week08)
- Lecture #9:  [Marlowe](code/week09)
- Lecture #10: [Uniswap](code/week10)

## Exercises

- Week #1

  - Clone the [The Plutus repository](https://github.com/input-output-hk/plutus), check out the correct commit
    as specified in [cabal.project](code/week01/cabal.project).
  - Install NixOS cross-referencing the following resources.
     - https://nixos.org/download.html
     - https://docs.plutus-community.com
     - A few resources to understand the what and why regarding NixOS
       - https://nixos.org/manual/nix/stable
       - https://serokell.io/blog/what-is-nix
  - Set-up IOHK binary caches [How to set up the IOHK binary caches](https://github.com/input-output-hk/plutus#iohk-binary-cache). "If you do not do this, you will end up building GHC, which takes several hours. If you find yourself building GHC, STOP and fix the cache."
  - Enter a `nix-shell`.
  - Build the [English Auction](code/week01) contract with `cabal build` (you may need to run `cabal update` first).
  - Go to the `plutus-playground-client` folder.
  - Start the Playground server with `plutus-playground-server`.
  - Start the Playground client (in another `nix-shell`) with `npm run start`.
  - Copy-paste the auction contract into the Playground editor.
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

  - Add a deadline to the minting policy in the [Homework1](code/week05/src/Week05/Homework1.hs) module.
  - Fix the token name to the empty ByteString in the NFT contract in the [Homework2](code/week05/src/Week05/Homework2.hs) module.

- Week #6

  - Get the Oracle demo running and extend it in some way.

- Week #7

  - Implement the game of "Rock, Paper, Scissors" using state machines.

- Week #8

  - Add a new operation close to the TokenSale-contract that allows the seller to close the contract and retrieve all remaining funds.
  - Modify the tests accordingly.

- Week #9
  - Modify the example Marlowe contract, so that Charlie must put down twice the deposit in the very beginning, which gets split between Alice and Bob if Charlie refuses to make his choice.

- Week #10
  - Get the Uniswap demo running and extend it in some way.

## Some Plutus Modules

- [`Language.Marlowe.Semantics`](https://github.com/input-output-hk/plutus/blob/master/marlowe/src/Language/Marlowe/Semantics.hs), contains Marlowe types and semantics.
- [`Plutus.Contract.StateMachine`](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Contract/StateMachine.hs), contains types and functions for using state machines.
- [`Plutus.Contract.Test`](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Contract/Test.hs), provides various ways to write tests for Plutus contracts.
- [`Plutus.Contract.Test.ContractModel`](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Contract/Test/ContractModel.hs), support for property based testing of Plutus contracts.
- [`Plutus.Contracts.Uniswap`](https://github.com/input-output-hk/plutus/blob/master/plutus-use-cases/src/Plutus/Contracts/Uniswap.hs), an implementation of Uniswap in Plutus.
- [`Plutus.Ledger.TimeSlot`](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger/src/Ledger/TimeSlot.hs), conversions between `Slot` and `POSIXTime`.
- [`Plutus.PAB.Webserver.API`](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/src/Plutus/PAB/Webserver/API.hs), contains the HTTP-interface provided by the PAB.
- [`Plutus.Trace.Emulator`](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Trace/Emulator.hs), contains types and functions related to traces.
- [`Plutus.V1.Ledger.Ada`](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Ada.hs), contains support for the Ada currency.
- [`Plutus.V1.Ledger.Contexts`](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Contexts.hs), contains the definition of the context-related types.
- [`Plutus.V1.Ledger.Interval`](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Interval.hs), contains the definition of and helper functions for the `Interval` type.
- [`Plutus.V1.Ledger.Slot`](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Slot.hs), contains the definition of the `Slot` type.
- [`Plutus.V1.Ledger.Value`](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Value.hs), contains the definition of and helper functions for the `Value` type.
- [`PlutusTx.Builtins.Data`](https://github.com/input-output-hk/plutus/blob/master/plutus-core/plutus-core/src/PlutusCore/Data.hs), contains the definition of the `Data` type.
- [`PlutusTx.IsData.Class`](https://github.com/input-output-hk/plutus/blob/master/plutus-tx/src/PlutusTx/IsData/Class.hs), defines the `IsData` class.

## Additional Resources

- [The Plutus repository](https://github.com/input-output-hk/plutus)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- [Haskell & Cryptocurrencies course Mongolia](https://www.youtube.com/playlist?list=PLJ3w5xyG4JWmBVIigNBytJhvSSfZZzfTm)
