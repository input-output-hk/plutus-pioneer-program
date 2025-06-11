<h1 align="center">
  <br>
  <a href="https://www.youtube.com/@iogacademy"><img src="https://ucarecdn.com/288e5001-d93e-4081-976b-0c6f72cc077e/iohksymbolbig.jpg" alt="IOG Academy on YouTube" width="100"></a>
  <br>
  Welcome to the Plutus Pioneers Program 
  <br>
</h1>

The Plutus Pioneer Program (PPP) is a course delivered by the IOG Education team
to recruit and train software developers in Plinth, the Haskell-embedded smart contract 
language that compiles to Plutus. 

## On-chain code 

This branch contains to PlutusV3 translated validator code from the 
[4th iteration](https://github.com/input-output-hk/plutus-pioneer-program/tree/fourth-iteration) 
of the Pioneer program. Some additional validator examples are added. The code 
in this branch can be compiled with a nix shell provided by the 
[plinth-template](https://github.com/IntersectMBO/plinth-template/tree/main) 
repository. Compilation was tested with Nix version `2.25.3` and `plinth-template`
repository commit: `4adea2ef260a99ec9d36de609579ae5208ac8c10`. 

Instructions how to setup a Plinth development environment with Nix, Docker or Demeter 
can be found at the [plinth-dev-env](https://github.com/iohkedu/plinth-dev-env/tree/main) 
repository. Once the environment is setup the validator code can be compiled with the 
following commands: 

```console
cabal update
cabal run 
```

The second command will generate the `blueprints.json` file that contains the Plutus 
blueprints of this repo including the compiled validator code. A compiled blueprint file 
is included in this branch. One can read more about Plutus blueprints in 
[CIP-57](https://cips.cardano.org/cip/CIP-57). 

## Off-chain code 

This branch is **work-in-progress**. Off-chain code is currently provided for validators up to 
`week05`. The code works on Cardano `preview` network. Comments in the code link the off-chain 
code to on-chain code and provide additional information. The code examples use two different 
libraries: [MeshJS](https://meshjs.dev/) and 
[Lucid Evolution](https://anastasia-labs.github.io/lucid-evolution/). Both libraries need to 
be installed together with [Deno](https://deno.com/), a runtime environment for JavaScript/TypeScript. 

The code was tested with Deno version `2.1.9`, MeshSDK package version `1.9.0-beta.3` 
and Lucid Evolution package version `0.4.22`. Use [npm](https://www.npmjs.com/) to install 
those packages. To fix a package version one can create the `package.json` file before 
installing the packages. Below is an example of this file: 

```console
{
  "dependencies": {
    "@meshsdk/core": "1.9.0-beta.3",
    "@meshsdk/common": "1.9.0-beta.3",
    "@meshsdk/core-cst": "1.9.0-beta.3",
    "@lucid-evolution/lucid": "0.4.22",
    "deno": "2.1.9"
  }
}
```

To run the off-chain code first create the `seed.ts` file that contains the seed of your 
test wallet. You can create a test wallet with [Lace](https://www.lace.io/) and fund it on test 
network with the [Cardano faucet](https://docs.cardano.org/cardano-testnets/tools/faucet). 
The wallet should have `ada` on `preview` network. The `seed.ts` file should contain the 
secret seed in the following form: 

* for MeshJS
```console
export const secretSeed = ["<seed1>", ..., "<seedN>"]
```
* for Lucid Evolution
```console
export const secretSeed = "<seed1> ... <seedN>"
```

Copy the `seed.ts` file into the folder from which you will run the off-chain code and 
execute: 

```console
deno run -A <file_name>.ts
```
