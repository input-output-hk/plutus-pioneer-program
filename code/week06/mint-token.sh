#!/bin/bash

amt=$1
tn=$2
echo "minting $1 coins of token $2"

curl -X 'POST' \
  'http://localhost:9080/api/contract/activate' \
  -H 'accept: application/json;charset=utf-8' \
  -H 'Content-Type: application/json;charset=utf-8' \
  -d '{
    "caWallet": {"getWalletId": "7cc75497535877261173ab585f5abb431f7ba484"},
    "caID": {
        "contents": {
            "tpToken": {"unTokenName": "'"$tn"'"},
            "tpAddress": {
                "addressStakingCredential": {
                    "contents": {
                        "contents": {"getPubKeyHash": "afabbcc3e3954c2e66329cb25cfa4c82bbfc22555eda55905b0d57ba"},
                        "tag": "PubKeyCredential"
                    },
                    "tag": "StakingHash"
                },
                "addressCredential": {
                    "contents": {"getPubKeyHash": "a51a69c16eeedc5d9bc469431b4ecffd80332120ffdd9ebd222cc315"},
                    "tag": "PubKeyCredential"
                }
            },
            "tpAmount": '"$1"'
        },
        "tag": "Mint"
    }
}'
