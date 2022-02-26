#!/bin/bash

cardano-wallet serve \
    --testnet testnet/testnet-byron-genesis.json \
    --node-socket $CARDANO_NODE_SOCKET_PATH
