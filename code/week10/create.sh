#1/bin/sh

symbol=$( cat symbol.json )
body="{\"cpAmountA\":$2,\"cpAmountB\":$4,\"cpCoinB\":{\"unAssetClass\":[$symbol,{\"unTokenName\":\"$5\"}]},\"cpCoinA\":{\"unAssetClass\":[$symbol,{\"unTokenName\":\"$3\"}]}}"
echo $body

curl  "http://localhost:8080/api/new/contract/instance/$(cat W$1.cid)/endpoint/create" \
--header 'Content-Type: application/json' \
--data-raw $body
