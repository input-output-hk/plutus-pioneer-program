#!/bin/bash

cardano-cli query utxo --address $(cat ./assets/gift.addr) --testnet-magic ${CARDANO_NODE_MAGIC}