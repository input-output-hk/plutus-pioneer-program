#!/bin/bash

txin=$1
tmp=/workspace/code/Week08/tmp
amt1=$(/workspace/code/Week08/scripts/query-stake-address-info-user1-script.sh | jq .[0].rewardAccountBalance)
amt2=$(expr $amt1 / 2 + 1)
pp=$tmp/protocol-params.json
body=$tmp/tx.txbody
signed=$tmp/tx.tx

echo "txin = $1"
echo "amt1 = $amt1"
echo "amt2 = $amt2"

export CARDANO_NODE_SOCKET_PATH=/workspace/cardano-private-testnet-setup/private-testnet/node-spo1/node.sock

cardano-cli query protocol-parameters \
    --testnet-magic 42 \
    --out-file $pp

cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 42 \
    --change-address $(cat $tmp/user1-script.addr) \
    --out-file $body \
    --tx-in $txin \
    --tx-in-collateral $txin \
    --tx-out "$(cat /workspace/cardano-private-testnet-setup/private-testnet/addresses/payment2.addr)+$amt2 lovelace" \
    --withdrawal "$(cat $tmp/user1-script-stake.addr)+$amt1" \
    --withdrawal-script-file /workspace/code/Week08/assets/staking.plutus \
    --withdrawal-redeemer-file /workspace/code/Week08/assets/unit.json \
    --protocol-params-file $pp

cardano-cli transaction sign \
    --testnet-magic 42 \
    --tx-body-file $body \
    --out-file $signed \
    --signing-key-file /workspace/cardano-private-testnet-setup/private-testnet/stake-delegator-keys/payment1.skey

cardano-cli transaction submit \
    --testnet-magic 42 \
    --tx-file $signed
