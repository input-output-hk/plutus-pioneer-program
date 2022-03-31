#!/bin/bash
export CARDANO_NODE_SOCKET_PATH=cardano-private-testnet-setup/private-testnet/node-bft1/node.sock
cardano-cli query stake-address-info \
    --testnet-magic 42 \
    --address $(cat tmp/user1-script-stake.addr)
