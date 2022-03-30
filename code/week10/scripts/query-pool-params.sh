#!/bin/bash
export CARDANO_NODE_SOCKET_PATH=cardano-private-testnet-setup/private-testnet/node-bft1/node.sock
cardano-cli query pool-params --testnet-magic 42 --stake-pool-id=$(scripts/query-stake-pools.sh)
