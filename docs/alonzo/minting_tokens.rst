Minting Tokens
==============

.. include:: ../notes/setup.rst

We will mint a native token.

Create Policy
-------------

First, we will create a policy that allows a new wallet, ``minter`` to mint tokens.

.. code:: bash

    ./createPolicy.sh minter

This will create ``minter.skey`` and ``minter.vkey`` in the ``wallets`` directory, and ``minter.script`` in the ``policies`` directory.

Fund a Minting Wallet
---------------------

Next, we will add some funds to the ``wallet2`` wallet.

.. code:: bash

    ./sendFromWallet.sh main
    Wallet Name:  main
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    b5e89af6cc624465c169349c5217fac20d6c9193a2df04ae63bd8a21f15d5ece     0        899899659010 lovelace + TxOutDatumHashNone
    TX row number: 1
    Lovelace to send: 1000000000
    Receiving wallet name: wallet2
    Transaction successfully submitted.

Then check the balance until it arrives.

.. code:: bash

    ./balance.sh wallet2
    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    fbe3d7e3d3b802eaa2a1e9f4df2a017796abb2c7dd83a476f04f6b92391adea8     1        1000000000 lovelace + TxOutDatumHashNone    

Mint Tokens
-----------

Now, we will mint 100 ``Chess`` tokens in ``wallet2``.

.. code:: bash

    ./mint.sh minter Chess 100 wallet2
    100000000
    Wallet Name:  wallet2
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    fbe3d7e3d3b802eaa2a1e9f4df2a017796abb2c7dd83a476f04f6b92391adea8     1        1000000000 lovelace + TxOutDatumHashNone
    TX row number: 1
    Transaction successfully submitted.    

This ``mint.sh`` script builds and executes the following commands.

.. code:: bash

    $CARDANO_CLI transaction build \
    --tx-in ${FROM_UTXO} \
    --tx-out ${FROM_WALLET_ADDRESS}+$TOKEN_COUNT+"$TOKEN_COUNT ${POLICY_ID}.${COIN_NAME}" \
    --change-address=${FROM_WALLET_ADDRESS} \
    --mint="$TOKEN_COUNT ${POLICY_ID}.${COIN_NAME}" \
    --mint-script-file="./policies/$POLICY_NAME.script" \
    --testnet-magic ${TESTNET_MAGIC_NUM} \
    --out-file tx.build \
    --witness-override 2 \
    --alonzo-era
    
    $CARDANO_CLI transaction sign \
    --tx-body-file tx.build \
    --signing-key-file ./wallets/${FROM_WALLET_NAME}.skey \
    --signing-key-file ./wallets/${POLICY_NAME}.skey \
    --out-file tx.signed
    
    $CARDANO_CLI transaction submit --tx-file tx.signed --testnet-magic $TESTNET_MAGIC_NUM
    
Then we can wait for the tokens to arrive.

.. code:: bash

    ./balance.sh wallet2
    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    2064de58563aab461f4deb9a8414558699361252b5a655d7c0ad23e1d90595fd     0        899822003 lovelace + TxOutDatumHashNone
    2064de58563aab461f4deb9a8414558699361252b5a655d7c0ad23e1d90595fd     1        100000000 lovelace + 100000000 a5bdebd0371758aeeb3b116432724fc6bf6c9caf186485baee7ee4d9.Chess + TxOutDatumHashNone

We can use the ``burn.sh`` script to burn some of the tokens in a UTxO. 

First, create the wallet.

.. code:: bash

    ./createWallet.sh bernie

Then add some Ada.

.. code:: bash

    ./sendFromWallet.sh main
    Wallet Name:  main
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    d22a8f5fc2bac7b851f14899e9af0556e394d884d0cddf2b2e29f76866a3310f     1        100000000 lovelace + 100000000 be40ec5a43dc88640f8ce1b7262b32918581ff9d6891d94bf4315ba5 + TxOutDatumHashNone
    f66a19065873436b0d131493f544ed6e7287acc65d3627d6551bcdae39685fdd     0        888899324928 lovelace + TxOutDatumHashNone
    TX row number: 2
    Lovelace to send: 10000000000
    Receiving wallet name: bernie
    Transaction successfully submitted.

Wait a while for it to arrive.

.. code:: bash
    
    ./balance.sh bernie
    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    a24f7d302ff49974463638259df93010087e0067edee11171072322e7b3e2123     1        10000000000 lovelace + TxOutDatumHashNone

Then mint ``100`` ``Rook`` tokens.

.. code:: bash

    /mint.sh minter Rook 100 bernie
    Wallet Name:  bernie
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    a24f7d302ff49974463638259df93010087e0067edee11171072322e7b3e2123     1        10000000000 lovelace + TxOutDatumHashNone
    TX row number: 1
    Transaction successfully submitted.

Wait for it to arrive.

.. code:: bash

    ./balance.sh bernie
    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    7e9cdb8f12c88100a431537ed8274d021f29d11a07ca5f31c66702f2e94f46cd     0        9899824555 lovelace + TxOutDatumHashNone
    7e9cdb8f12c88100a431537ed8274d021f29d11a07ca5f31c66702f2e94f46cd     1        100000000 lovelace + 100000000 a5bdebd0371758aeeb3b116432724fc6bf6c9caf186485baee7ee4d9.Rook + TxOutDatumHashNone

Then burn ``10`` ``Rook`` tokens.

.. code:: bash

    ./burn.sh minter Rook 10 bernie
    10000000
    Wallet Name:  bernie
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    7e9cdb8f12c88100a431537ed8274d021f29d11a07ca5f31c66702f2e94f46cd     0        9899824555 lovelace + TxOutDatumHashNone
    7e9cdb8f12c88100a431537ed8274d021f29d11a07ca5f31c66702f2e94f46cd     1        100000000 lovelace + 100000000 a5bdebd0371758aeeb3b116432724fc6bf6c9caf186485baee7ee4d9.Rook + TxOutDatumHashNone
    TX row number: 2
    Transaction successfully submitted.
    
And, once the transaction has been executed, we will see the ``bernie`` wallet has only ``90`` of the ``Rook`` tokens left.

.. code:: bash

    ./balance.sh bernie
    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    7e9cdb8f12c88100a431537ed8274d021f29d11a07ca5f31c66702f2e94f46cd     0        9899824555 lovelace + TxOutDatumHashNone
    894154c7322ef79a7f94db08066b00eb842b9636d3bf712a4f0addd7c7fbf303     0        89824555 lovelace + TxOutDatumHashNone
    894154c7322ef79a7f94db08066b00eb842b9636d3bf712a4f0addd7c7fbf303     1        10000000 lovelace + 90000000 a5bdebd0371758aeeb3b116432724fc6bf6c9caf186485baee7ee4d9.Rook + TxOutDatumHashNone


