#!/bin/bash

vkey=cardano-private-testnet-setup/private-testnet/addresses/user2.vkey \
skey=cardano-private-testnet-setup/private-testnet/addresses/user2.skey \

export CARDANO_NODE_SOCKET_PATH=cardano-private-testnet-setup/private-testnet/node-bft1/node.sock
cardano-cli address key-gen \
    --verification-key-file $vkey \
    --signing-key-file $skey
cardano-cli address build \
    --testnet-magic 42 \
    --payment-verification-key-file $vkey \
    --out-file cardano-private-testnet-setup/private-testnet/addresses/user2.addr
