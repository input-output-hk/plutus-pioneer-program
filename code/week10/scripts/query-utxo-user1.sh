#!/bin/bash
export CARDANO_NODE_SOCKET_PATH=cardano-private-testnet-setup/private-testnet/node-bft1/node.sock
cardano-cli query utxo \
    --testnet-magic 42 \
    --address $(cat cardano-private-testnet-setup/private-testnet/addresses/user1.addr)
