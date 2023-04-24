# Run simple transaction

At this point, we have a private testnet running and 3 wallet addresses. You may also have a db-sync process attached to it, if applicable.
Let's run a simple payment transaction and see what the resulting data
looks like in the `cardano-db-sync` schema.

## 1. Create keys and addresses for another wallet

- Create new keys and wallet address for wallet4
  ```shell
  cd $HOME/src/cardano-private-testnet-setup
  
  ROOT=private-testnet
  export CARDANO_NODE_SOCKET_PATH=$ROOT/node-spo1/node.sock
  ADDR=wallet4
  
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
- set `TXIN` variable with first utxo from wallet1.addr
  ```shell
  # run query
  cardano-cli query utxo --address $(cat ${ROOT}/addresses/wallet1.addr) --testnet-magic 42
  # example output
                             TxHash                                 TxIx        Amount
  --------------------------------------------------------------------------------------
  eed6b3a208608440d635b2a49ea0a7cbf1807ac3fb3ff39e7653d960e691660d     0        1200000000000 lovelace + TxOutDatumNone
  
  # set variable to first utxo - TxHash#TxIx
  # e.g., TXIN=eed6b3a208608440d635b2a49ea0a7cbf1807ac3fb3ff39e7653d960e691660d#0
  TXIN=<TxHash>#<TxIndex>
  ```
- perform transaction to consume wallet1 UTXO to send 5 ADA to wallet4 and send change to wallet1
  ```shell  
  SEND_AMT=5000000
  TXOUT_ADDR2=$(cat ${ROOT}/addresses/wallet4.addr)+${SEND_AMT}    
  CURRENT_SLOT=$(cardano-cli query tip --testnet-magic 42 | jq -r '.slot')
  
  cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 42 \
    --change-address $(cat $ROOT/addresses/wallet1.addr) \
    --tx-in $TXIN \
    --tx-out $TXOUT_ADDR2 \
    --invalid-hereafter $(( ${CURRENT_SLOT} + 10000)) \
    --protocol-params-file protocol-parameters.json \
    --out-file tx.body
  
  cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file $ROOT/addresses/wallet1.skey \
    --testnet-magic 42 \
    --out-file tx.signed
    
  cardano-cli transaction submit --tx-file tx.signed --testnet-magic 42
  
  # check UTXOs for wallet1.addr to confirm new UTXO with change amount
  cardano-cli query utxo --address $(cat ${ROOT}/addresses/wallet1.addr) --testnet-magic 42
  
  # output
                               TxHash                                 TxIx        Amount
  --------------------------------------------------------------------------------------
  53349feec7afc2c20213fa0061676ec943243e30a591a21225b7585e82ddc255     1        1199994833047 lovelace + TxOutDatumNone
  
  # check UTXOs for wallet4.addr to confirm new UTXO
  cardano-cli query utxo --address $(cat ${ROOT}/addresses/wallet4.addr) --testnet-magic 42
  
  # output
                               TxHash                                 TxIx        Amount
  --------------------------------------------------------------------------------------
  53349feec7afc2c20213fa0061676ec943243e30a591a21225b7585e82ddc255     0        5000000 lovelace + TxOutDatumHashNone
  
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
  11
  (1 row)
  # query the fee amount for the transaction
  select tx.fee from tx where tx.id = 11;
  # sample output
  fee   
  ---------
  166953
  (1 row)
  # query the transaction outputs related to the transaction
  select tx_out.id, tx_out.tx_id, tx_out.index, tx_out.address, tx_out.value 
    from tx_out inner join tx on tx_out.tx_id = tx.id where tx.id = 11;
  # sample output
  id | tx_id | index |                                                   address                                                    |   value   
  ----+-------+-------+--------------------------------------------------------------------------------------------------------------+-----------
  10 |    11 |     0 | addr_test1qr7uwen2ux7g6ucwf3hs69n307c463xttkk2sy04tn70vcx6n5ejt6snhgyl65m49t49qug7fjlktzf90xhspfnkkreq853x4z |       5000000
  11 |    11 |     1 | addr_test1vpruhn2tm7h9kcal36lrcs6zfxxlusjka7lxhx6us606laq4qe8jw                                              | 1199994833047
  (2 rows)
  ```

