#!/bin/bash

cardano-cli query utxo \
   $MAGIC \
   --address $(cat 02.addr)

