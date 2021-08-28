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



