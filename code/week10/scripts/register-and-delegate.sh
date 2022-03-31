#!/bin/bash

addr=$(cat tmp/user2.addr) \
txin=$1
echo "addr: $addr"
echo "txin: $txin"

script=tmp/stake-validator.script
script_stake_addr=tmp/user1-script-stake.addr
script_payment_addr=tmp/user1-script.addr
registration=tmp/registration.cert
delegation=tmp/delegation.cert
pp=tmp/protocol-params.json
raw=tmp/tx.raw
signed=tmp/tx.signed

export CARDANO_NODE_SOCKET_PATH=cardano-private-testnet-setup/private-testnet/node-bft1/node.sock

cabal run write-stake-validator -- $script $addr

cardano-cli stake-address build \
    --testnet-magic 42 \
    --stake-script-file $script \
    --out-file $script_stake_addr

echo "stake address: $(cat $script_stake_addr)"

cardano-cli address build \
    --testnet-magic 42 \
    --payment-verification-key-file=cardano-private-testnet-setup/private-testnet/addresses/user1.vkey \
    --stake-script-file=$script \
    --out-file $script_payment_addr

echo "payment address: $(cat $script_payment_addr)"

cardano-cli stake-address registration-certificate \
    --stake-script-file $script \
    --out-file $registration

cardano-cli stake-address delegation-certificate \
    --stake-script-file $script \
    --stake-pool-id=$(scripts/query-stake-pools.sh) \
    --out-file $delegation

cardano-cli query protocol-parameters \
    --testnet-magic 42 \
    --out-file $pp

cardano-cli transaction build \
    --testnet-magic 42 \
    --change-address $(cat $script_payment_addr) \
    --out-file $raw \
    --tx-in $txin \
    --tx-in-collateral $txin \
    --certificate-file $registration \
    --certificate-file $delegation \
    --certificate-script-file $script \
    --certificate-redeemer-file unit.json \
    --protocol-params-file $pp

cardano-cli transaction sign \
    --testnet-magic 42 \
    --tx-body-file $raw \
    --out-file $signed \
    --signing-key-file cardano-private-testnet-setup/private-testnet/addresses/user1.skey

cardano-cli transaction submit \
    --testnet-magic 42 \
    --tx-file $signed
