#!/bin/bash

cardano-cli query utxo \
   $MAGIC \
   --address $(cat testnet/02.addr)

