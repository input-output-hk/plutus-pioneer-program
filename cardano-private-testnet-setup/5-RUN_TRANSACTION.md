# Run simple transaction

At this point, we have a private testnet running. You may also have a db-sync process attached to it, if applicable.
Let's run a simple payment transaction and see what the resulting data
looks like in the `cardano-db-sync` schema.

## 1. Create keys and addresses for another user

- Create new keys and wallet address for user2
  ```shell
  cd $HOME/src/cardano-private-testnet-setup
  
  ROOT=private-testnet
  export CARDANO_NODE_SOCKET_PATH=$ROOT/node-bft1/node.sock
  ADDR=user2
  
  # payment address keys
  cardano-cli address key-gen \
  --verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
  --signing-key-file      ${ROOT}/addresses/${ADDR}.skey

  # stake address keys
  cardano-cli stake-address key-gen \
  --verification-key-file ${ROOT}/addresses/${ADDR}-stake.vkey \
  --signing-key-file      ${ROOT}/addresses/${ADDR}-stake.skey

  # wallet address
  cardano-cli address build \
  --payment-verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
  --stake-verification-key-file ${ROOT}/addresses/${ADDR}-stake.vkey \
  --testnet-magic 42 \
  --out-file ${ROOT}/addresses/${ADDR}.addr  

  # stake address
  cardano-cli stake-address build \
  --stake-verification-key-file ${ROOT}/addresses/${ADDR}-stake.vkey \
  --testnet-magic 42 \
  --out-file ${ROOT}/addresses/${ADDR}-stake.addr

  # stake addresses registration cert
  cardano-cli stake-address registration-certificate \
  --stake-verification-key-file ${ROOT}/addresses/${ADDR}-stake.vkey \
  --out-file ${ROOT}/addresses/${ADDR}-stake.reg.cert
  ```

## 2. Make a payment transaction
- get network protocol parameters
  ```shell
  cardano-cli query protocol-parameters \
  --testnet-magic 42 \
  --out-file protocol-parameters.json
  ```
- set `TXIN` variable with first utxo from user1.addr
  ```shell
  # run query
  cardano-cli query utxo --address $(cat ${ROOT}/addresses/user1.addr) --testnet-magic 42
  # example output
                               TxHash                                 TxIx        Amount
  --------------------------------------------------------------------------------------
  b0f91ee59eb208284467b1dec0adfa8c57eb1cf7587fb7eb0599e2b8c8e885c9     0        500000000 lovelace + TxOutDatumHashNone
  b0f91ee59eb208284467b1dec0adfa8c57eb1cf7587fb7eb0599e2b8c8e885c9     1        500000000 lovelace + TxOutDatumHashNone
  
  # set variable to first utxo - TxHash#TxIx
  # e.g., TXIN=b0f91ee59eb208284467b1dec0adfa8c57eb1cf7587fb7eb0599e2b8c8e885c9#0
  TXIN=<TxHash>#0
  ```
- perform transaction to consume user1 UTXO to send 5 ADA to user2 wallet and send change to user1 wallet
  ```shell  
  SEND_AMT=5000000
  TXOUT_ADDR2=$(cat ${ROOT}/addresses/user2.addr)+${SEND_AMT}    
  CURRENT_SLOT=$(cardano-cli query tip --testnet-magic 42 | jq -r '.slot')
  
  cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 42 \
    --change-address $(cat $ROOT/addresses/user1.addr) \
    --tx-in $TXIN \
    --tx-out $TXOUT_ADDR2 \
    --invalid-hereafter $(( ${CURRENT_SLOT} + 10000)) \
    --protocol-params-file protocol-parameters.json \
    --out-file tx.body
  
  cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file $ROOT/addresses/user1.skey \
    --testnet-magic 42 \
    --out-file tx.signed
    
  cardano-cli transaction submit --tx-file tx.signed --testnet-magic 42
  
  # check UTXOs for user1.addr to confirm new UTXO with change amount
  cardano-cli query utxo --address $(cat ${ROOT}/addresses/user1.addr) --testnet-magic 42
  
  # output
                               TxHash                                 TxIx        Amount
  --------------------------------------------------------------------------------------
  510926b57d1427abafd29aa9c77192ff165db44d19e014a9770df9431d741d9a     1        494831639 lovelace + TxOutDatumHashNone
  b0f91ee59eb208284467b1dec0adfa8c57eb1cf7587fb7eb0599e2b8c8e885c9     1        500000000 lovelace + TxOutDatumHashNone
  
  # check UTXOs for user2.addr to confirm new UTXO
  cardano-cli query utxo --address $(cat ${ROOT}/addresses/user2.addr) --testnet-magic 42
  
  # output
                               TxHash                                 TxIx        Amount
  --------------------------------------------------------------------------------------
  510926b57d1427abafd29aa9c77192ff165db44d19e014a9770df9431d741d9a     0        5000000 lovelace + TxOutDatumHashNone
  
  ```

## 3. Query the db-sync schema (assuming you have set up db-sync)

- open a psql prompt
  ```shell
  # set the postgres env. variable
  export PGPASSFILE=postgres-conn/pgpass-privatenet
  # log-in to the privatenet database with our linux account
  psql -d privatenet
  # sample output
  psql (13.4 (Ubuntu 13.4-0ubuntu0.21.04.1))
  Type "help" for help.
  privatenet=#
  ```
- run queries in the psql editor
  ```shell
  # query transaction table and find id of the transaction we submitted above, i.e. one with max block_id
  select tx.id from tx where tx.block_id = (select max (tx.block_id) from tx);
  # sample output  
  id
  ----
  9
  (1 row)
  # query the fee amount for the transaction
  select tx.fee from tx where tx.id = 9;
  # sample output
  fee   
  ---------
  1000000
  (1 row)
  # query the transaction outputs related to the transaction
  select tx_out.id, tx_out.tx_id, tx_out.index, tx_out.address, tx_out.value 
    from tx_out inner join tx on tx_out.tx_id = tx.id where tx.id = 9;
  # sample output
  id | tx_id | index |                                                   address                                                    |   value   
  ----+-------+-------+--------------------------------------------------------------------------------------------------------------+-----------
  13 |     9 |     0 | addr_test1qzj7xja5rrly33z570ukrq7ayd3amf24k77v4dhu96fyal0vsegs7g7vwzwnrspq4eysyr66sc0wcv2xdtpau68m5qvqxhnvc5 |   5000000
  14 |     9 |     1 | addr_test1qp6h5v5ysaa0uqmhsmmu3gr8nhsf4w3st95p0vvvp4ldzn7esqup0hdw7rsuuhcyk0qrrvfp2yr4vtxhh0276yrcpmqqanu7lp | 494000000
  (2 rows)
  ```

Continue to next guide: [6. Run Plutus script transactions](6-RUN_PLUTUS_SCRIPT_TXS.md)
