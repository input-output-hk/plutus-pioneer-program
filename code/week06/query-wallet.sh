#!/bin/bash

export CARDANO_NODE_SOCKET_PATH=testnet/node.sock
cardano-cli query utxo \
   --testnet-magic 1097911063 \
   --address addr_test1qzj356wpdmhdchvmc355xx6wel7cqvepyrlam84aygkvx9d04w7v8cu4fshxvv5ukfw05nyzh07zy427mf2eqkcd27aqax2r7e

