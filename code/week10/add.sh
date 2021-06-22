#1/bin/sh

symbol=$( cat symbol.json )
body="{\"apAmountA\":$2,\"apAmountB\":$4,\"apCoinB\":{\"unAssetClass\":[$symbol,{\"unTokenName\":\"$5\"}]},\"apCoinA\":{\"unAssetClass\":[$symbol,{\"unTokenName\":\"$3\"}]}}"
echo $body

curl  "http://localhost:8080/api/new/contract/instance/$(cat W$1.cid)/endpoint/add" \
--header 'Content-Type: application/json' \
--data-raw $body
