#!/bin/bash

txHash = $1
txIndex = $2

# Build gift address 
cardano-cli address build \
    --payment-script-file ./assets/gift.plutus \
    --testnet-magic ${CARDANO_NODE_MAGIC} \
    --out-file ./assets/gift.addr

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic ${CARDANO_NODE_MAGIC} \
    --tx-in 39b2c0324c72b366b8a8d9335679049bed94e44d8033161d275d355d7a401d7c#0 \ # TODO: use args
    --tx-out $(cat ./assets/gift.addr)+3000000 \
    --tx-out-datum-hash-file ./assets/unit.json \
    --change-address $(cat ./assets/user.addr) \
    --out-file ./assets/gift-tx.body
    
# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file ./assets/gift-tx.body \
    --signing-key-file ./assets/user.skey \
    --testnet-magic ${CARDANO_NODE_MAGIC} \
    --out-file ./assets/tx.signed

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic ${CARDANO_NODE_MAGIC} \
    --tx-file ./assets/tx.signed