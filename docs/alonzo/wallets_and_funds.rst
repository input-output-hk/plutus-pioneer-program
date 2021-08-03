Wallets and Funds
=================

Some Helper Scripts
___________________

I have a repo that contains a few helper scripts that I use. It's rough and ready, but saves a little time for some common tasks.

.. code:: bash
    
    cd /data
    git clone https://github.com/chris-moreton/plutus-scripts

Generate test addresses
_______________________

Run the generate wallets script to generate a few addresses.

.. code:: bash

    cd /data/plutus-scripts
    ./generateAddresses.sh

This will create some ``.addr``, ``.skey`` and ``.vkey`` files in the ``wallets`` directory.    

Use the Faucet
______________

If you have access to the testnet faucet, transfer some test Ada to the ``main`` wallet.

Then, check that it has arrived. It should arrive within a minute or so.

.. code:: bash

    cd /data/plutus-scripts
    ./balance.sh main

    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    40f0fa60a71e247e3eca46147fc159080aa7667763ae8c3be00b2e48400bbccd     0        1000000000000 lovelace + TxOutDatumHashNone

Transfer some funds
____________________

We will transfer some funds to ``wallet1``. This uses another helper script, which takes the sending wallet as an argument and then asks for the UTxO, amount and receiving wallet name.

.. code:: bash

    ./sendFromWallet.sh main

    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    40f0fa60a71e247e3eca46147fc159080aa7667763ae8c3be00b2e48400bbccd     0        1000000000000 lovelace + TxOutDatumHashNone

    TX row number: 1
    Lovelace to send: 1000000000
    Receiving wallet name: wallet1

    Transaction successfully submitted.

Check that it has arrived.

.. code:: bash

    ./balance.sh wallet1

    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    bd7422ef2cd55d1c5a33601a3b75b080bc3742856e5ddb8dfdfae07f583c7af1     0        1000000000 lovelace + TxOutDatumHashNone
