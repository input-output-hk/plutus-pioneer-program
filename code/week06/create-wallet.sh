#!/bin/bash

name=$1
passphrase=$2
file=$3
echo "creating wallet with name $name passphrase $passphrase"

phrase=$(cardano-wallet recovery-phrase generate)

x=''
sep=''
for word in $phrase
do
    x=$x$sep'"'$word'"'
    sep=', '
done

cat > $file <<- EOM
{ "name": "$name"
, "mnemonic_sentence": [$x]
, "passphrase": "$passphrase"
}
EOM
echo "saved restoration file to $file"
