#!/bin/bash

amt=$1
tn=$2
echo "minting $1 coins of token $2"

cabal run mint-token -- $1 $2 $WALLETID $ADDRESS
