#!/bin/bash
echo "name?"
read name

path=/workspaces/plutus-pioneer-program/keys
vkey="$path/$name.vkey"
skey="$path/$name.skey"
addr="$path/$name.addr"

cardano-cli address key-gen --verification-key-file "$vkey" --signing-key-file "$skey"
cardano-cli address build --payment-verification-key-file "$vkey" --testnet-magic 2 --out-file "$addr"

echo "wrote verification key to: $vkey"
echo "wrote signing key to: $skey"
echo "wrote address to: $addr"