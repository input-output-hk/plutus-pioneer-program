#!/bin/bash

cabal run -- oracle-pab \
  --config testnet/pab-config.yml migrate
