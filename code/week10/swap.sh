#1/bin/sh

symbol=$( cat symbol.json )
body="{\"spAmountA\":$2,\"spAmountB\":0,\"spCoinB\":{\"unAssetClass\":[$symbol,{\"unTokenName\":\"$4\"}]},\"spCoinA\":{\"unAssetClass\":[$symbol,{\"unTokenName\":\"$3\"}]}}"
echo $body

curl  "http://localhost:8080/api/new/contract/instance/$(cat W$1.cid)/endpoint/swap" \
--header 'Content-Type: application/json' \
--data-raw $body
