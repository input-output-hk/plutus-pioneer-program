#1/bin/sh

symbol=$( cat symbol.json )
body="{\"rpCoinB\":{\"unAssetClass\":[$symbol,{\"unTokenName\":\"$4\"}]},\"rpDiff\":$2,\"rpCoinA\":{\"unAssetClass\":[$symbol,{\"unTokenName\":\"$3\"}]}}"
echo $body

curl  "http://localhost:8080/api/new/contract/instance/$(cat W$1.cid)/endpoint/remove" \
--header 'Content-Type: application/json' \
--data-raw $body
