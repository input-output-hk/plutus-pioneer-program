#!/bin/bash

curl -H "content-type: application/json" \
    -XGET localhost:8090/v2/wallets/$WALLET_ID/addresses | jq '.'
