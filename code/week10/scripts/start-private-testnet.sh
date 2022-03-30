#!/bin/bash

if [ -d tmp ];
then
    rm -rf tmp
fi
mkdir tmp

cd cardano-private-testnet-setup
scripts/automate.sh
