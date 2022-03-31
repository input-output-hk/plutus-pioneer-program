#!/bin/bash

txin=$1
amt=$(scripts/query-stake-address-info-user1.sh | jq .[0].rewardAccountBalance)
raw=tmp/tx.raw
signed=tmp/tx.signed

echo "txin = $1"
echo "amt = $amt"

export CARDANO_NODE_SOCKET_PATH=cardano-private-testnet-setup/private-testnet/node-bft1/node.sock

cardano-cli transaction build \
    --testnet-magic 42 \
    --change-address $(cat cardano-private-testnet-setup/private-testnet/addresses/user1.addr) \
    --out-file $raw \
    --tx-in $txin \
    --withdrawal "$(cat cardano-private-testnet-setup/private-testnet/addresses/user1-stake.addr)+$amt" \

cardano-cli transaction sign \
    --testnet-magic 42 \
    --tx-body-file $raw \
    --out-file $signed \
    --signing-key-file cardano-private-testnet-setup/private-testnet/addresses/user1.skey \
    --signing-key-file cardano-private-testnet-setup/private-testnet/addresses/user1-stake.skey

cardano-cli transaction submit \
    --testnet-magic 42 \
    --tx-file $signed
