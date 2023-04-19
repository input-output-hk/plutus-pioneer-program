#!/usr/bin/env bash

SCRIPT_PATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"

ROOT=private-testnet

export PGPASSFILE=postgres-conn/pgpass-privatenet

# define socket path to one of the block producers (picking node-bft1 is arbitrary)
export CARDANO_NODE_SOCKET_PATH=${ROOT}/node-spo1/node.sock

# copy the dbsync-config.yaml from template into $ROOT
cp "${SCRIPT_PATH}"/../templates/db-sync-config-template.yaml ${ROOT}/db-sync-config.yaml

cardano-db-sync \
    --config ${ROOT}/db-sync-config.yaml \
    --socket-path ${CARDANO_NODE_SOCKET_PATH} \
    --state-dir ${ROOT}/ledger-state/privatenet \
    --schema-dir ${SCHEMA_DIR}
