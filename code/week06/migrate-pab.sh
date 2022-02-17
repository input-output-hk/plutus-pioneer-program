#!/bin/bash

cabal exec -- oracle-pab \
  --config testnet/pab-config.yml migrate
