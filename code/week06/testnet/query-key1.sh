#!/bin/bash

cardano-cli query utxo \
   $MAGIC \
   --address $(cat 01.addr)

