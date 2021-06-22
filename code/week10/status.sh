#1/bin/sh
curl  "http://localhost:8080/api/new/contract/instance/$(cat W$1.cid)/status" | jq ".cicCurrentState.observableState"
