# Execute transactions for Plutus script example

In this guide, we run transactions to demonstrate a vesting contract.
The Plutus validator contains the business logic to decide if a spending transaction may grab some ADA that is locked at the script.
In particular, it ensures that only the intended beneficiary may grab the ADA and only after a specific deadline has passed.
The beneficiary public key hash and the deadline parameters are encoded as JSON in a Datum input file.

The code for this example comes from the [Plutus Pioneer Program github repo-Week 3](https://github.com/input-output-hk/plutus-pioneer-program/tree/main/code/week03/src/Week03).
The `DeployVesting.hs` code has been adapted from the Plutus Pioneer program code to allow parameter inputs to be passed in to the function for the datum json output.

Be sure to watch the [video from Lars Bruenjes](https://www.youtube.com/watch?v=ABtffZPoUqU&list=PLNEK_Ejlx3x2zxcfoVGARFExzOHwXFCCL&index=7) 
that walks through the process of executing transactions with a plutus validator script.
This video provides important background information about how transactions are constructed.
That kind of background information is not covered by this guide.

The instructions below do not mirror the instructions from the video, but have been adapted slightly to fit this project.
For instance, in the example of the Plutus Pioneer program, Lars used a parameterized validator, whereas this guide is
passing parameters using a Datum file.

## Obtain beneficiary and deadline values
In order to generate the JSON datum file, we will need to pass arguments for the beneficiary public key hash and the deadline timestamp.
This section shows how to get these argument values, so we can store them in a notes document to be used in a later step.

### Obtain the public key hash of user2 wallet verification key, which will be the beneficiary of the gift from user1
**Note**: These directions assume the following:
1. You are running a private cardano testnet, and it is fully initialized to the latest
   era and major protocol. If not, follow the directions in [3. Run network scripts](3-RUN_NETWORK_SCRIPTS.md).
2. You have created the address keys for user2 already by following the directions in [5. Run transaction directions](5-RUN_TRANSACTION.md).

Either use terminal session from the previous guide or start a new terminal session
```shell
cd $HOME/src/cardano-private-testnet-setup

# set a ROOT testnet home variable if necessary and export the socket path
ROOT=private-testnet
export CARDANO_NODE_SOCKET_PATH=$ROOT/node-bft1/node.sock

# generate a public-key-hash output file for user2
cardano-cli address key-hash --payment-verification-key-file ${ROOT}/addresses/user2.vkey

# copy the output value to a notes document for future reference
```

### Get the UNIX epoch value for the deadline we want to use in the vesting example
1. We can visit an online converter such as [epoch converter](https://www.epochconverter.com/) to get this value 
2. In the epoch converter site, input a deadline in the calendar inputs row and copy the value computed in
   the input box under the label `Convert epoch to human-readable date and vice versa`
3. Paste this value into a notes document for future reference   

## Compile the script-example project code
We will run `cabal build` inside of `nix-shell` to take advantage of the prebuilt IOG binaries in their `nix` repo
You will need to have `nix` installed to do this. Using `nix` saves a huge amount of time as
compared to compiling all the IOG dependencies directly from Haskell sources.

### Setup Nix on your local machine if not installed already

This [Plutus community guide](https://docs.plutus-community.com/docs/setup/Ubuntu.html) 
has instructions to setup `nix` and configure the IOG binary caches in your nix configuration.

**Note**: The author chose to use a single user install due to running into problems with multi-user setup.

### Start a nix-shell from `plutus-apps` project in a new terminal
```shell
# change to a working directory for your project sources
cd $HOME/src

# clone the IOG plutus-apps repo if you don't have it already
git clone https://github.com/input-output-hk/plutus-apps.git

# if you already have plutus-apps repo, do a git pull to ensure you have latest tag data
cd plutus-apps
git pull

# checkout the tag defined in the cabal.project file under the plutus-apps source repo package
git checkout 4edc082309c882736e9dec0132a3c936fe63b4ea

# From the root plutus-apps directory, run nix-shell to start a nix-shell session
# This will also build plutus-apps project, so it takes quite a while the first time you build it
nix-shell
```

## Generate the JSON files for datum and redeemer inputs and the serialized Plutus validator file
```shell
# the following instructions assume you are inside of the nix-shell you started in plutus-apps repo
cd $HOME/src/cardano-private-testnet-setup/script-example/project

# run cabal build 
cabal build

# start a cabal repl session
cabal repl

# generate the redeemer JSON. The redeemer input is the Haskell unit type, i.e. ()
writeUnit "out/redeemer.json"

# verify a file called redeemer.json is produced with constructor 0 and empty list of fields
# e.g., {"constructor":0,"fields":[]}

# generate the datum JSON using the user2 pkh and UNIX epoch value (copied to a notes file in earlier step)
writeVestingDatumJson "out/datum.json" "<user2-pkh-value>" <UNIX-epoch-value>

# verify a file called datum.json is produced with suitable constructor and fields
# e.g., {"constructor":0,"fields":[{"bytes":"af330d93f0ca889a2220adca6ef2365d1a19b99dbb91d6f4cc13c649"},{"int":1643474471}]}

# write the serialized form of the Plutus validator
writeVestingValidator "out/vesting.plutus"

# verify a vesting.plutus file is produced
```

## Run transaction to lock ADA from user1 at the script address
You can re-use the same terminal session you started earlier to obtain the public key hash of user2
or you can start a new terminal session as desired.

```shell
# change directory to root of the setup project if necessary
cd $HOME/src/cardano-private-testnet-setup/

# set a ROOT testnet home variable if necessary and export the socket path
ROOT=private-testnet
export CARDANO_NODE_SOCKET_PATH=$ROOT/node-bft1/node.sock

# copy the output files we generated in the previous step to the working directory 
cp script-example/project/out/redeemer.json .
cp script-example/project/out/datum.json .
cp script-example/project/out/vesting.plutus .

# get protocol parameters output file
cardano-cli query protocol-parameters \
--testnet-magic 42 \
--out-file "protocol-parameters.json"

# generate validator script address and output to file
cardano-cli address build \
--payment-script-file vesting.plutus \
--testnet-magic 42 \
--out-file script.addr

# set a variable to hold the tx_in UTXO value for user1
# query the utxo info for user1
cardano-cli query utxo --address $(cat ${ROOT}/addresses/user1.addr) --testnet-magic 42

# choose a UTXO to assign to tx_in, 
# e.g. tx_in=b0f91ee59eb208284467b1dec0adfa8c57eb1cf7587fb7eb0599e2b8c8e885c9#0
tx_in=<TxHash>#<TxIndex>

# Build a transaction to transfer 20 ADA from user1 to the script address and return any change to user1
cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 42 \
    --change-address $(cat $ROOT/addresses/user1.addr) \
    --tx-in $tx_in \
    --tx-out "$(cat script.addr) 200000000 lovelace" \
    --tx-out-datum-hash-file datum.json \
    --out-file tx.body

# The output should be something like this: Estimated transaction fee: Lovelace 297

# sign the transaction with user1 signing key 
cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file $ROOT/addresses/user1.skey \
    --testnet-magic 42 \
    --out-file tx.signed

# submit the transaction
cardano-cli transaction submit \
    --testnet-magic 42 \
    --tx-file tx.signed

# wait for a bit to make sure the transaction has been added to the blockchain
# query the UTXOs at the script address to make sure the transfer is successful
cardano-cli query utxo --address $(cat script.addr) --testnet-magic 42

# you should see a UTXO with 20 ADA in value
```
## Run transaction for user2 (the beneficary) that grabs the ADA gift from user1
We may continue using the same terminal session from the previous step 
```shell
# set a variable to hold the UTXO value at the script address
# query the UTXO information if necessary
cardano-cli query utxo --address $(cat script.addr) --testnet-magic 42

script_tx_in=<TxHash>#<TxIndex>

# set a variable to hold the UTXO value for User2, which will be used for collateral
# query the UTXO information if necessary
cardano-cli query utxo --address $(cat $ROOT/addresses/user2.addr) --testnet-magic 42

collateral_tx_in=<TxHash>#<TxIndex>

# make sure the deadline you chose has passed in order to see the transaction work
# get the current slot
current_slot=$(cardano-cli query tip --testnet-magic 42 | jq '.slot')

# set variable with the user2 public key hash from your notes document
signer_pkh=<copy value from your notes>

# build transaction to let user2 grab the UTXO at the script address
# pass the serialized Plutus script, datum & redeemer inputs, and provide signer hash  
cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 42 \
    --change-address $(cat $ROOT/addresses/user2.addr) \
    --tx-in $script_tx_in \
    --tx-in-script-file vesting.plutus \
    --tx-in-datum-file datum.json \
    --tx-in-redeemer-file redeemer.json \
    --tx-in-collateral $collateral_tx_in \
    --required-signer-hash $signer_pkh \
    --invalid-before $current_slot \
    --protocol-params-file protocol-parameters.json \
    --out-file tx.body

# sign the transaction with user2 signing key
cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file $ROOT/addresses/user2.skey \
    --testnet-magic 42 \
    --out-file tx.signed

# submit the transaction
cardano-cli transaction submit \
    --testnet-magic 42 \
    --tx-file tx.signed

# wait for a bit to make sure the transaction has been added to the blockchain
# query the UTXOs at user2 address to make sure the grab is successful
cardano-cli query utxo --address $(cat $ROOT/addresses/user2.addr) --testnet-magic 42

# verify user2 now has a new UTXO.  It will be less than 20 ADA because of the fees 
```