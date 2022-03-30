#!/bin/bash

txin=$1
amt1=$(scripts/query-stake-address-info-user1-script.sh | jq .[0].rewardAccountBalance)
amt2=$(expr $amt1 / 2 + 1)
pp=tmp/protocol-params.json
raw=tmp/tx.raw
signed=tmp/tx.signed

echo "txin = $1"
echo "amt1 = $amt1"
echo "amt2 = $amt2"

export CARDANO_NODE_SOCKET_PATH=cardano-private-testnet-setup/private-testnet/node-bft1/node.sock

cardano-cli query protocol-parameters \
    --testnet-magic 42 \
    --out-file $pp

cardano-cli transaction build \
    --testnet-magic 42 \
    --change-address $(cat tmp/user1-script.addr) \
    --out-file $raw \
    --tx-in $txin \
    --tx-in-collateral $txin \
    --tx-out "$(cat tmp/user2.addr)+$amt2 lovelace" \
    --withdrawal "$(cat tmp/user1-script-stake.addr)+$amt1" \
    --withdrawal-script-file tmp/stake-validator.script \
    --withdrawal-redeemer-file unit.json \
    --protocol-params-file $pp

cardano-cli transaction sign \
    --testnet-magic 42 \
    --tx-body-file $raw \
    --out-file $signed \
    --signing-key-file cardano-private-testnet-setup/private-testnet/addresses/user1.skey

cardano-cli transaction submit \
    --testnet-magic 42 \
    --tx-file $signed
