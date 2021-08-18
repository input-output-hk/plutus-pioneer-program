#1/bin/sh

symbol=$( cat symbol.json )
body="{\"clpCoinB\":{\"unAssetClass\":[$symbol,{\"unTokenName\":\"$3\"}]},\"clpCoinA\":{\"unAssetClass\":[$symbol,{\"unTokenName\":\"$2\"}]}}"
echo $body

curl  "http://localhost:9080/api/contract/instance/$(cat W$1.cid)/endpoint/close" \
--header 'Content-Type: application/json' \
--data-raw $body
