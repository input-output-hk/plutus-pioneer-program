#!/bin/bash
export CARDANO_NODE_SOCKET_PATH=/workspace/cardano-private-testnet-setup/private-testnet/node-spo1/node.sock
cardano-cli query stake-address-info \
    --testnet-magic 42 \
    --address $(cat /workspace/cardano-private-testnet-setup/private-testnet/addresses/staking1.addr)
