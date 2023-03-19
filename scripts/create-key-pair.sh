#!/bin/bash

if [ -z "$1" ]; then
    >&2 echo "expected name as argument"
    exit 1
fi

path=/workspace/keys
mkdir -p "$path"

vkey="$path/$1.vkey"
skey="$path/$1.skey"
addr="$path/$1.addr"

if [ -f "$vkey" ]; then
    >&2 echo "verification key file $vkey already exists"
    exit 1
fi

if [ -f "$skey" ]; then
    >&2 echo "signing key file $skey already exists"
    exit 1
fi

if [ -f "$addr" ]; then
    >&2 echo "address file $addr already exists"
    exit 1
fi

cardano-cli address key-gen --verification-key-file "$vkey" --signing-key-file "$skey" &&
cardano-cli address build --payment-verification-key-file "$vkey" --testnet-magic 2 --out-file "$addr"

echo "wrote verification key to: $vkey"
echo "wrote signing key to: $skey"
echo "wrote address to: $addr"