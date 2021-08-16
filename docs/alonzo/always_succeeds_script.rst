AlwaysSucceeds Script
=====================

.. include:: ../notes/setup.rst

Plutus scripts get compiled down to the following format. 

.. code:: json

    {
        "type": "PlutusScriptV1",
        "description": "",
        "cborHex": "4e4d01000033222220051200120011"
    } 
    
This is the ``AlwaysSucceeds.plutus`` script whose validator always succeeds, which means that anyone will be able to consume any UTxO sitting at its address. The script can be found in the ``/data/plutus-scripts/scripts`` directory. 

Pay to the Script
-----------------

Using the ``payToScript.sh`` helper script, you can send ``99000000`` lovelace from ``wallet1`` to the ``AlwaysSucceeds.plutus`` script with a datum of ``6666``, allowing for fees of ``200000``.

The helper script ``payToScript.sh`` constructs a transaction, and, as one of its inputs, it reads the ``AlwaysSucceeds.plutus`` file in the ``scripts`` directory.

.. code:: bash

    ./payToScript.sh 99000000 AlwaysSucceeds 6666 wallet1

    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    bd7422ef2cd55d1c5a33601a3b75b080bc3742856e5ddb8dfdfae07f583c7af1     0        1000000000 lovelace + TxOutDatumHashNone
    TX row number: 1
    Transaction successfully submitted.

The ``payToScript.sh`` script wraps the following commands.

.. code:: bash

    $CARDANO_CLI transaction build \
        --tx-in ${SELECTED_UTXO} \
        --tx-out ${TO_ADDR}+${PAYMENT} \
        --tx-out-datum-hash ${DATUM_HASH} \
        --change-address=${FROM_ADDR} \
        --testnet-magic $TESTNET_MAGIC_NUM \
        --out-file tx.build \
        --alonzo-era
    
    $CARDANO_CLI transaction sign \
        --tx-body-file tx.build \
        --signing-key-file ./wallets/${SELECTED_WALLET_NAME}.skey \
        --testnet-magic $TESTNET_MAGIC_NUM \
        --out-file tx.signed \
    
    $CARDANO_CLI transaction submit --tx-file tx.signed --testnet-magic $TESTNET_MAGIC_NUM

Check that the funds arrive in the script using the ``contractBalance.sh`` script. You may see a lot of UTxOs sitting at the ``AlwaysSucceeds`` script address and hopefully
one of them will be yours.

The UTxOs at this address are now *locked* in the sense that they are guarded by a validator. The script address is the hash of the validator.

.. code:: bash

    ./contractBalance.sh AlwaysSucceeds

    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    063a62b69e51296417687077f8df67f1b2fe1568830ad56fb0f04d22739e69e2     0        5000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "b7a4cc0f36854309590c132e75dad06a4f6045e57ac93e6dafc9bf0d0018247d"
    44412566ec42af806660fe9846a71b50eae1b7028116a3d666cab3ba1f02d7ee     0        1000000000000 lovelace + TxOutDatumHashNone
    56382a3e1789df882114b2322787f1785eac71b19675ee88fd1dc6ca807ddc02     0        999888777 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"
    843f4ffa4aafc5ed968d0a9f0fb8a203796b66327343246bfd8d4ca1d361c2f8     0        99000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"
    8657ff66828f90ab7d45fb2e9f10286d9887f49bc83f7cf3d7b45e8fd1068aaf     0        10000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9e1199a988ba72ffd6e9c269cadb3b53b5f360ff99f112d9b2ee30c4d74ad88b"
    8c5f24a4eee17773d2ddef2ee1493248b1c45c56e6851d6f330deee1dc23a21f     0        1010011010 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "915e807fa63409181d1533195753e3170587b1edc089be670ab483da8f9bcd48"
    8f75351368cc2521315ac9908f0532a00e996e35644cbd9db4d616a7122c7491     0        979199655182 lovelace + TxOutDatumHashNone
    f441da5a5f04ee6057a98650bf4c2a4931906e37acfd2d705cb208eda48cef92     0        10000000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "df5078aee07dd171a343fb99d5fc1b5462fb3c94d82bf72dc1b77d9c0aceec29"

The ``contractBalance.sh`` script is an extension of the ``balance.sh``. It just has the extra step of determining the address of the contract.

In this case, the balance of UTxO number 4 is ``99000000`` and the datum hash is ``9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710``. We can check that this is the
correct datum hash.

.. code:: bash

    $CARDANO_CLI transaction hash-script-data --script-data-value 6666
    9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710

If there are too many, you could use ``grep`` to filter out the ones with the correct balance.

.. code:: bash

    ./contractBalance.sh AlwaysSucceeds | grep 99000000
    843f4ffa4aafc5ed968d0a9f0fb8a203796b66327343246bfd8d4ca1d361c2f8     0        99000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"

Unlock the Funds in the Script
------------------------------

When trying to consume UTxOs locked in a script, you need to provide collateral that will cover the costs if validation fails. For this we can use a separate wallet for storing
collateral UTxOs, to keep things tidy. One of the several wallets we created earlier on was named ``fees``, which we will use for fees and for collateral.

Under normal circumstances, collateral should never be lost because the wallet can perform validation in a deterministic fashion and only
submit the transaction if validation is guaranteed to pass.

.. code:: bash

    ./sendFromWallet.sh main

                            TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    bd7422ef2cd55d1c5a33601a3b75b080bc3742856e5ddb8dfdfae07f583c7af1     1        998999800000 lovelace + TxOutDatumHashNone
    TX row number: 1  
    Lovelace to send: 100000000000
    Receiving wallet name: fees
    Transaction successfully submitted.

We should wait a few moments and then check that it's arrived in our ``fees`` wallet.

.. code:: bash

    ./balance.sh fees
                            TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    7678d8d6b95ed026d7c690fb53419bdaa580cb00c56450ac3bd97712dd71ca4e     0        1000000000 lovelace + TxOutDatumHashNone

The following command will try to get the UTxO from the script using a datum of ``6666``, which is the correct datum. The ``42`` is an arbitrary value for the redeemer, but this value is ignored here.

.. code:: bash

    ./getFromScript.sh AlwaysSucceeds 6666 42 wallet1
    ============================================================================================
    Select Script UTxO
    ============================================================================================
    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    063a62b69e51296417687077f8df67f1b2fe1568830ad56fb0f04d22739e69e2     0        5000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "b7a4cc0f36854309590c132e75dad06a4f6045e57ac93e6dafc9bf0d0018247d"
    44412566ec42af806660fe9846a71b50eae1b7028116a3d666cab3ba1f02d7ee     0        1000000000000 lovelace + TxOutDatumHashNone
    56382a3e1789df882114b2322787f1785eac71b19675ee88fd1dc6ca807ddc02     0        999888777 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"
    843f4ffa4aafc5ed968d0a9f0fb8a203796b66327343246bfd8d4ca1d361c2f8     0        99000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"
    8657ff66828f90ab7d45fb2e9f10286d9887f49bc83f7cf3d7b45e8fd1068aaf     0        10000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9e1199a988ba72ffd6e9c269cadb3b53b5f360ff99f112d9b2ee30c4d74ad88b"
    8c5f24a4eee17773d2ddef2ee1493248b1c45c56e6851d6f330deee1dc23a21f     0        1010011010 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "915e807fa63409181d1533195753e3170587b1edc089be670ab483da8f9bcd48"
    8f75351368cc2521315ac9908f0532a00e996e35644cbd9db4d616a7122c7491     0        979199655182 lovelace + TxOutDatumHashNone
    f441da5a5f04ee6057a98650bf4c2a4931906e37acfd2d705cb208eda48cef92     0        10000000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "df5078aee07dd171a343fb99d5fc1b5462fb3c94d82bf72dc1b77d9c0aceec29"

    TX row number: 4
    ============================================================================================
    Select Collateral UTxO
    ============================================================================================
    Wallet Name: fees
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    7678d8d6b95ed026d7c690fb53419bdaa580cb00c56450ac3bd97712dd71ca4e     0        1000000000 lovelace + TxOutDatumHashNone
    TX row number: 1

    Transaction successfully submitted.

The ``getFromScript.sh`` script wraps the following commands.

.. code:: bash

    $CARDANO_CLI query protocol-parameters --testnet-magic $TESTNET_MAGIC_NUM > params.json

    $CARDANO_CLI transaction build \
        --tx-in ${COLLATERAL_TX} \
        --tx-in ${SCRIPT_UTXO} \
        --tx-in-datum-value "${DATUM_VALUE}" \
        --tx-in-redeemer-value "${REDEEMER_VALUE}" \
        --tx-in-script-file $SCRIPT_FILE \
        --tx-in-collateral=${COLLATERAL_TX} \
        --change-address=${FEE_ADDR} \
        --tx-out ${TO_ADDR}+${PAYMENT} \
        --tx-out-datum-hash ${DATUM_HASH} \
        --out-file tx.build \
        --testnet-magic $TESTNET_MAGIC_NUM \
        --protocol-params-file "params.json" \
        --alonzo-era
    
    $CARDANO_CLI transaction sign \
        --tx-body-file tx.build \
        --signing-key-file ./wallets/${SIGNING_WALLET}.skey \
        --testnet-magic $TESTNET_MAGIC_NUM \
        --out-file tx.signed \
    
    $CARDANO_CLI transaction submit --tx-file tx.signed --testnet-magic $TESTNET_MAGIC_NUM
    
Let's check that it arrived in ``wallet1`` as expected.

.. code:: bash

    ./balance.sh wallet1

    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    ee22529028220bb2d2cbda634fbe982602afd5baf7f173341e2c8f9157e2912d     1        99000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"


