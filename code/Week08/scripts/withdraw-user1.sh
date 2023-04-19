#!/bin/bash

txin=$1
amt=$(/workspace/code/Week08/scripts/query-stake-address-info-user1.sh | jq .[0].rewardAccountBalance)
body=/workspace/code/Week08/tmp/tx.txbody
signed=/workspace/code/Week08/tmp/tx.tx

echo "txin = $1"
echo "amt = $amt"

export CARDANO_NODE_SOCKET_PATH=/workspace/cardano-private-testnet-setup/private-testnet/node-spo1/node.sock

cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 42 \
    --change-address $(cat /workspace/cardano-private-testnet-setup/private-testnet/addresses/payment1.addr) \
    --out-file $body \
    --tx-in "$txin" \
    --withdrawal "$(cat /workspace/cardano-private-testnet-setup/private-testnet/addresses/staking1.addr)+$amt" \

cardano-cli transaction sign \
    --testnet-magic 42 \
    --tx-body-file $body \
    --out-file $signed \
    --signing-key-file /workspace/cardano-private-testnet-setup/private-testnet/stake-delegator-keys/payment1.skey \
    --signing-key-file /workspace/cardano-private-testnet-setup/private-testnet/stake-delegator-keys/staking1.skey

cardano-cli transaction submit \
    --testnet-magic 42 \
    --tx-file $signed
