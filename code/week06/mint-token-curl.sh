#!/bin/bash

amt=$1
tn=$2
echo "minting $1 coins of token $2"

pkh=$(cabal exec payment-key-hash -- $ADDRESS)
skh=$(cabal exec stake-key-hash -- $ADDRESS)
echo "payment key hash: $pkh"
echo "stake key hash: $skh"

curl -X 'POST' \
  'http://localhost:9080/api/contract/activate' \
  -H 'accept: application/json;charset=utf-8' \
  -H 'Content-Type: application/json;charset=utf-8' \
  -d '{
    "caWallet": {"getWalletId": "'"$WALLETID"'"},
    "caID": {
        "contents": {
            "tpToken": {"unTokenName": "'"$tn"'"},
            "tpAddress": {
                "addressCredential": {
                    "contents": {"getPubKeyHash": "'"$pkh"'"},
                    "tag": "PubKeyCredential"
                },
                "addressStakingCredential": {
                    "contents": {
                        "contents": {"getPubKeyHash": "'"$skh"'"},
                        "tag": "PubKeyCredential"
                    },
                    "tag": "StakingHash"
                }
            },
            "tpAmount": '"$amt"'
        },
        "tag": "Mint"
    }
}'
