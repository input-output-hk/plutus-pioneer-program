#!/bin/bash

vkey=tmp/user2.vkey \
skey=tmp/user2.skey \

export CARDANO_NODE_SOCKET_PATH=cardano-private-testnet-setup/private-testnet/node-bft1/node.sock
cardano-cli address key-gen \
    --verification-key-file $vkey \
    --signing-key-file $skey
cardano-cli address build \
    --testnet-magic 42 \
    --payment-verification-key-file $vkey \
    --out-file tmp/user2.addr
