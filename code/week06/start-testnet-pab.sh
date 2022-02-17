#!/bin/bash

cabal exec -- oracle-pab \
  --config testnet/pab-config.yml webserver \
  --passphrase pab123456789
