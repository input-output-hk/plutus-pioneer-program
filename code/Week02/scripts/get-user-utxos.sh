#!/bin/bash

cardano-cli query utxo --address $(cat ./assets/user.addr) --testnet-magic ${CARDANO_NODE_MAGIC}