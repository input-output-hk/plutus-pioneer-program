#!/bin/bash

cabal run -- token-pab \
  --config testnet/pab-config.yml migrate
