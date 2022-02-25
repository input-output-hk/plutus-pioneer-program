#!/bin/bash

oref=$1
amt=$2
tn=$3
addrFile=$4
skeyFile=$5

echo "oref: $oref"
echo "amt: $amt"
echo "tn: $tn"
echo "address file: $addrFile"
echo "signing key file: $skeyFile"

ppFile=testnet/protocol-parameters.json
cardano-cli query protocol-parameters $MAGIC --out-file $ppFile

policyFile=testnet/token.plutus
cabal exec token-policy $policyFile $oref $amt $tn

unsignedFile=testnet/tx.unsigned
signedFile=testnet/tx.signed
pid=$(cardano-cli transaction policyid --script-file $policyFile)
tnHex=$(cabal exec token-name -- $tn)
addr=$(cat $addrFile)
v="$amt $pid.$tnHex"

echo "currency symbol: $pid"
echo "token name (hex): $tnHex"
echo "minted value: $v"
echo "address: $addr"

cardano-cli transaction build \
    $MAGIC \
    --tx-in $oref \
    --tx-in-collateral $oref \
    --tx-out "$addr + 1500000 lovelace + $v" \
    --mint "$v" \
    --mint-script-file $policyFile \
    --mint-redeemer-file testnet/unit.json \
    --change-address $addr \
    --protocol-params-file $ppFile \
    --out-file $unsignedFile \

cardano-cli transaction sign \
    --tx-body-file $unsignedFile \
    --signing-key-file $skeyFile \
    $MAGIC \
    --out-file $signedFile

cardano-cli transaction submit \
    $MAGIC \
    --tx-file $signedFile
