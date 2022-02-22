#!/bin/bash

symbol="71066256d2f4850c731819a8e6de155c97ea1ee53dd3dd903f8e3258"
token="USDT"

echo "running oracle for currency symbol $symbol and token name $token"

curl -X 'POST' \
  'http://localhost:9080/api/contract/activate' \
  -H 'accept: application/json;charset=utf-8' \
  -H 'Content-Type: application/json;charset=utf-8' \
  -d '{
    "caWallet": {"getWalletId": "7cc75497535877261173ab585f5abb431f7ba484"},
    "caID": {
        "contents": {
            "opFees": 1000000,
            "opToken": {"unTokenName": "'"$token"'"},
            "opAddress": {
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
            "opSymbol": {"unCurrencySymbol": "'"$symbol"'"}
        },
        "tag": "Oracle"
    }
}'
