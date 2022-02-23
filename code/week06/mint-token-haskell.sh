#!/bin/bash

amt=$1
tn=$2
echo "minting $1 coins of token $2"

cabal run mint-token -- $1 $2 7cc75497535877261173ab585f5abb431f7ba484 addr_test1qzj356wpdmhdchvmc355xx6wel7cqvepyrlam84aygkvx9d04w7v8cu4fshxvv5ukfw05nyzh07zy427mf2eqkcd27aqax2r7e
