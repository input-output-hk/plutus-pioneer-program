#!/bin/bash

cardano-cli query utxo \
   $MAGIC \
   --address $(cat testnet/01.addr)

