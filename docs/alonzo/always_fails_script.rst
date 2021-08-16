AlwaysFails Script
==================

.. include:: ../notes/setup.rst

Now we will try similar transactions with a script whose validator always fails. This time we will lose our collateral.

Sending
-------

First, lock some lovelace in the AlwaysFails script which will be picked up from the ``./scripts`` directory of the cloned ``plutus-scripts`` repository.

.. code:: bash

    ./payToScript.sh 99000000 AlwaysFails 6666 wallet1
    Wallet Name:  wallet1
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    060aa2af10655a4b893bb4b828aa2288a5a18f1dd8941a7f99ffe8d3bd1d71f1     1        99000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"
    c1658d652d7e318ae990da7973a0bbca6d787130e079102a16c1b3568ccfe8df     0        99900831551 lovelace + TxOutDatumHashNone
    TX row number: 2
    Transaction successfully submitted.
    
    
Check that it has arrived.

.. code:: bash

    ./contractBalance.sh AlwaysFails

    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    0913d72c55b3d6e765eb51f1a5da1436ea0554a89d499fe3398d20517f0b455e     0        99000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"
    29f82f40603c4328e6efffb7c6e8851fe9540d18ddc0930b188896f6b016e141     0        100000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "ee5c9e2778c6c398366c5b9cfd67a888081f7626ca0ac392faca5981e59ff759"
    5589e823cf148597cbf64dc7cb5ebcd3957d5fc83c3521b281daa9f9c490c8ab     0        999888777 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"

Grabbing
--------

Now we try to get some funds from the script, but it can't succeed.

.. code:: bash

    ./getFromScript.sh AlwaysFails 6666 42 wallet1
    ============================================================================================
    Select Script UTxO
    ============================================================================================
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    0913d72c55b3d6e765eb51f1a5da1436ea0554a89d499fe3398d20517f0b455e     0        99000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"
    29f82f40603c4328e6efffb7c6e8851fe9540d18ddc0930b188896f6b016e141     0        100000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "ee5c9e2778c6c398366c5b9cfd67a888081f7626ca0ac392faca5981e59ff759"
    5589e823cf148597cbf64dc7cb5ebcd3957d5fc83c3521b281daa9f9c490c8ab     0        999888777 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"
    TX row number: 1
    ============================================================================================
    Select Collateral UTxO
    ============================================================================================
    Wallet Name: fees
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    ee22529028220bb2d2cbda634fbe982602afd5baf7f173341e2c8f9157e2912d     0        889819803 lovelace + TxOutDatumHashNone
    TX row number: 1

    Command failed: transaction build  Error: The following scripts have execution failures:
    the script for transaction input 1 (in the order of the TxIds) failed with The Plutus script evaluation failed: An error has occurred:  User error:
    The provided Plutus code called 'error'.

    Command failed: transaction submit  Error: Error while submitting tx: ShelleyTxValidationError ShelleyBasedEraAlonzo (ApplyTxError [UtxowFailure (WrappedShelleyEraFailure (UtxoFailure (ValueNotConservedUTxO (Value 0 (fromList [])) (Value 99900831551 (fromList []))))),UtxowFailure (WrappedShelleyEraFailure (UtxoFailure (BadInputsUTxO (fromList [TxInCompact (TxId {_unTxId = SafeHash "c1658d652d7e318ae990da7973a0bbca6d787130e079102a16c1b3568ccfe8df"}) 0]))))])
