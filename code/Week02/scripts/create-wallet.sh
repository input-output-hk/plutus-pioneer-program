#!/bin/bash

# Generate key-pair
cardano-cli address key-gen \
--verification-key-file ./assets/user.vkey \
--signing-key-file ./assets/user.skey

# Use key-pair to generate walled address
cardano-cli address build \
--payment-verification-key-file ./assets/user.vkey \
--out-file ./assets/user.addr \
--testnet-magic ${CARDANO_NODE_MAGIC}