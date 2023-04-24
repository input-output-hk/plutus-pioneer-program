#!/usr/bin/env bash

set -e

ROOT=private-testnet
export CARDANO_NODE_SOCKET_PATH=${ROOT}/node-spo1/node.sock

pushd $ROOT

mkdir -p addresses

# create soft links of the utxo vkey and utxo skey to wallet equivalent names
ln -s ../utxo-keys/utxo1.vkey ./addresses/wallet1.vkey
ln -s ../utxo-keys/utxo1.skey ./addresses/wallet1.skey

ln -s ../utxo-keys/utxo2.vkey ./addresses/wallet2.vkey
ln -s ../utxo-keys/utxo2.skey ./addresses/wallet2.skey

ln -s ../utxo-keys/utxo3.vkey ./addresses/wallet3.vkey
ln -s ../utxo-keys/utxo3.skey ./addresses/wallet3.skey

cardano-cli address build --testnet-magic 42 --payment-verification-key-file ./addresses/wallet1.vkey --out-file ./addresses/wallet1.addr
echo "Created wallet1 address from UTxO1"

cardano-cli address build --testnet-magic 42 --payment-verification-key-file ./addresses/wallet2.vkey --out-file ./addresses/wallet2.addr
echo "Created wallet2 address from UTxO2"

cardano-cli address build --testnet-magic 42 --payment-verification-key-file ./addresses/wallet3.vkey --out-file ./addresses/wallet3.addr
echo "Created wallet3 address from UTxO3"

popd