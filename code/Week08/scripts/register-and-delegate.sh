#!/bin/bash

tmp=/workspace/code/Week08/tmp
txin=$1
echo "txin: $txin"

script=/workspace/code/Week08/assets/staking.plutus
script_stake_addr=$tmp/user1-script-stake.addr
script_payment_addr=$tmp/user1-script.addr
registration=$tmp/registration.cert
delegation=$tmp/delegation.cert
pp=$tmp/protocol-params.json
body=$tmp/tx.txbody
signed=$tmp/tx.tx

export CARDANO_NODE_SOCKET_PATH=/workspace/cardano-private-testnet-setup/private-testnet/node-spo1/node.sock

cardano-cli stake-address build \
    --testnet-magic 42 \
    --stake-script-file $script \
    --out-file $script_stake_addr

echo "stake address: $(cat $script_stake_addr)"

cardano-cli address build \
    --testnet-magic 42 \
    --payment-verification-key-file=/workspace/cardano-private-testnet-setup/private-testnet/stake-delegator-keys/payment1.vkey \
    --stake-script-file=$script \
    --out-file $script_payment_addr

echo "payment address: $(cat $script_payment_addr)"

cardano-cli stake-address registration-certificate \
    --stake-script-file $script \
    --out-file $registration

cardano-cli stake-address delegation-certificate \
    --stake-script-file $script \
    --stake-pool-id=$(/workspace/code/Week08/scripts/query-stake-pools.sh | head -n 1) \
    --out-file $delegation

cardano-cli query protocol-parameters \
    --testnet-magic 42 \
    --out-file $pp

cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 42 \
    --change-address $(cat $script_payment_addr) \
    --out-file $body \
    --tx-in $txin \
    --tx-in-collateral $txin \
    --certificate-file $registration \
    --certificate-file $delegation \
    --certificate-script-file $script \
    --certificate-redeemer-file /workspace/code/Week08/assets/unit.json \
    --protocol-params-file $pp

cardano-cli transaction sign \
    --testnet-magic 42 \
    --tx-body-file $body \
    --out-file $signed \
    --signing-key-file /workspace/cardano-private-testnet-setup/private-testnet/stake-delegator-keys/payment1.skey

cardano-cli transaction submit \
    --testnet-magic 42 \
    --tx-file $signed
