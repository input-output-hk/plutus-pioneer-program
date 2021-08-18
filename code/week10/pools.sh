#1/bin/sh
curl  "http://localhost:9080/api/contract/instance/$(cat W$1.cid)/endpoint/pools" \
--header 'Content-Type: application/json' \
--data-raw '[]'
