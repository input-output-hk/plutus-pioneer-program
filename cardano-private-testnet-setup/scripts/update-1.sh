#!/usr/bin/env bash

echo "Sending some utxos to user1's address..."
set -e

ROOT=example
FEE=1000000

SCRIPT_PATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
. "${SCRIPT_PATH}"/config-read.shlib; # load the config library functions

ROOT="$(config_get ROOT)";

pushd ${ROOT}

export CARDANO_NODE_SOCKET_PATH=node-bft1/node.sock

# find the greatest intput in the whole utxo and use it
GREATEST_INPUT=$(cardano-cli query utxo --whole-utxo --testnet-magic 42 | tail -n +3 | awk '{printf "%s#%s %s \n", $1 , $2, $3}' | sort -rn -k2 | head -n1)

TXID0=$(echo ${GREATEST_INPUT} | awk '{print $1}')
COINS_IN_INPUT=$(echo ${GREATEST_INPUT} | awk '{print $2}')

echo "Using ${TXID0}, containing ${COINS_IN_INPUT} lovelace"

cardano-cli transaction build-raw \
--alonzo-era \
--fee ${FEE} \
            --tx-in ${TXID0}\
            --tx-out $(cat addresses/user1.addr)+$((${COINS_IN_INPUT} / 2)) \
            --tx-out $(cat addresses/user1.addr)+$((${COINS_IN_INPUT} / 2 - ${FEE})) \
            --certificate-file addresses/pool-owner1-stake.reg.cert \
            --certificate-file node-pool1/registration.cert \
            --certificate-file addresses/user1-stake.reg.cert \
            --certificate-file addresses/user1-stake.deleg.cert \
            --out-file tx2.txbody

cardano-cli transaction sign \
            --signing-key-file shelley/utxo-keys/utxo1.skey \
            --signing-key-file addresses/user1-stake.skey \
            --signing-key-file node-pool1/owner.skey \
            --signing-key-file node-pool1/shelley/operator.skey \
            --signing-key-file shelley/genesis-keys/genesis1.skey \
            --signing-key-file shelley/genesis-keys/genesis2.skey \
            --signing-key-file shelley/delegate-keys/delegate1.skey \
            --signing-key-file shelley/delegate-keys/delegate2.skey \
            --testnet-magic 42 \
            --tx-body-file  tx2.txbody \
            --out-file      tx2.tx

cardano-cli transaction submit --tx-file tx2.tx --testnet-magic 42

popd
