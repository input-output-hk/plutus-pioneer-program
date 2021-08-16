Wallets and Funds
=================

Some Helper Scripts
___________________

I have a repo that contains a few helper scripts that I use. It's rough and ready, but saves a little time for some common tasks.

.. code:: bash
    
    cd /data
    git clone https://github.com/chris-moreton/plutus-scripts

Generate test addresses
-----------------------

Run the generate wallets script to generate a few addresses.

.. code:: bash

    cd /data/plutus-scripts
    ./generateAddresses.sh

This script wraps the following command.

.. code:: bash

    # example only, don't run this
    $CARDANO_CLI address key-gen --verification-key-file main.vkey --signing-key-file main.skey    

This will create some ``.addr``, ``.skey`` and ``.vkey`` files in the ``wallets`` directory.    

Use the Faucet
--------------

If you have access to the testnet faucet, transfer some test Ada to the ``main`` wallet.

.. code:: bash

    ./faucet.sh SECRET_KEY
    
Then, check that it has arrived. It should arrive within a minute or so.

.. code:: bash

    cd /data/plutus-scripts
    ./balance.sh main

    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    40f0fa60a71e247e3eca46147fc159080aa7667763ae8c3be00b2e48400bbccd     0        1000000000000 lovelace + TxOutDatumHashNone

The ``balance.sh`` script wraps the following command.

.. code:: bash

    # example only, don't run this
    $CARDANO_CLI query utxo --address $(cat ./wallets/$1.addr) --testnet-magic $TESTNET_MAGIC_NUM

Transfer some funds
-------------------

We will transfer some funds to ``wallet1``. This uses another helper script, which takes the sending wallet as an argument and then asks for the UTxO, amount and receiving wallet name.

.. code:: bash

    ./sendFromWallet.sh main

    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    40f0fa60a71e247e3eca46147fc159080aa7667763ae8c3be00b2e48400bbccd     0        1000000000000 lovelace + TxOutDatumHashNone

    TX row number: 1
    Lovelace to send: 100000000000
    Receiving wallet name: wallet1

    Transaction successfully submitted.

The ``sendFromWallet.sh`` script wraps the following commands.

.. code:: bash

    # example only, don't run these

    $CARDANO_CLI transaction build \
        --tx-in ${FROM_UTXO} \
        --tx-out ${TO_WALLET_ADDRESS}+${LOVELACE_TO_SEND} \
        --change-address=${FROM_WALLET_ADDRESS} \
        --testnet-magic ${TESTNET_MAGIC_NUM} \
        --out-file tx.build \
        --alonzo-era
    
    $CARDANO_CLI transaction sign \
        --tx-body-file tx.build \
        --signing-key-file ./wallets/${FROM_WALLET_NAME}.skey \
        --out-file tx.signed
    
    $CARDANO_CLI transaction submit --tx-file tx.signed --testnet-magic $TESTNET_MAGIC_NUM
    
   
Check that it has arrived.

.. code:: bash

    ./balance.sh wallet1

    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    bd7422ef2cd55d1c5a33601a3b75b080bc3742856e5ddb8dfdfae07f583c7af1     0        1000000000 lovelace + TxOutDatumHashNone
