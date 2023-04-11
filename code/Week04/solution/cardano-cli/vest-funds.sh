#!/bin/bash

assets=/workspace/code/Week04/assets
keypath=/workspace/keys

name="$1"
txin="$2"
script="$3"
datum="$4"
amount="$5"
body="$assets/vest.txbody"
tx="$assets/vest.tx"

# Build gift address 
cardano-cli address build \
    --payment-script-file "$assets/$script" \
    --testnet-magic 2 \
    --out-file "$assets/$script.addr"

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-out "$(cat "$assets/$script.addr") + $amount lovelace" \
    --tx-out-inline-datum-file "$assets/$datum" \
    --change-address "$(cat "$keypath/$name.addr")" \
    --out-file "$body"
    
# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file "$body" \
    --signing-key-file "$keypath/$name.skey" \
    --testnet-magic 2 \
    --out-file "$tx"

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file "$tx"

tid=$(cardano-cli transaction txid --tx-file "$tx")
echo "transaction id: $tid"
echo "Cardanoscan: https://preview.cardanoscan.io/transaction/$tid"
