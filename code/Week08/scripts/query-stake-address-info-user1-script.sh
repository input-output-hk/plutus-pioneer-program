#!/bin/bash
export CARDANO_NODE_SOCKET_PATH=/workspace/cardano-private-testnet-setup/private-testnet/node-spo1/node.sock
cardano-cli query stake-address-info \
    --testnet-magic 42 \
    --address $(cat /workspace/code/Week08/tmp/user1-script-stake.addr)
