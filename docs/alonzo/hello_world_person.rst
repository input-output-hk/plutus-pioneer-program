HelloWorld, ByteStrings and Redeemer
====================================

.. include:: ../notes/setup.rst

The next thing to run is a script that checks that the datum matches a given person's name and that the redeemer matches a given birthday.

A brief look at some of the code gives an idea as to how this works.

.. code:: haskell

    person :: PersonDetails
    person = PersonDetails { pName = "Sam Jones", pDob = "1974/12/23" }
    
    {-# INLINABLE helloWorld #-}
    
    helloWorld :: PersonDetails -> P.ByteString -> P.ByteString -> ScriptContext -> P.Bool
    helloWorld thePerson datum redeemer context =
        pName thePerson P.== datum     P.&&
        pDob thePerson  P.== redeemer

Examining the code will give an insight into how to use ``ByteString`` parameters. It will also show a basic parameterized contract script. Even though its parameters in this case
are hard-coded, they still have the effect of generating a different script address for different person names.

The code can be found in ``/data/plutus-scripts/plutus-sources/plutus-helloworld/src/Cardano/PlutusExample/HelloWorld``.

We can compile it as follows.

.. code:: bash

    cd /data/plutus-scripts/plutus-sources/plutus-helloworld/
    cabal run plutus-helloworld-person

This time the code will output a file with the name ``plutus-helloworld-person.plutus``. We will move this to our scripts directory.

.. code:: bash

    mv helloworld-person.plutus ../../scripts/HelloWorldPerson.plutus

Then we can pay some funds to this script.

.. code:: bash

    cd /data/plutus-scripts
    ./payToScript.sh 62500000 HelloWorldPerson "\"Sam Jones\"" wallet1
    Wallet Name:  wallet1
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    060aa2af10655a4b893bb4b828aa2288a5a18f1dd8941a7f99ffe8d3bd1d71f1     1        99000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"
    11248966667145ab998933075d4eca6305e96b58e03684c7d1c8100a410c17df     1        62500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "8fb8d1694f8180e8a59f23cce7a70abf0b3a92122565702529ff39baf01f87f1"
    7760e20988b77005a557e937e493aaf98103ec46e6b9ddc90ed27485bf8602d0     0        99706326204 lovelace + TxOutDatumHashNone
    TX row number: 3
    Transaction successfully submitted.    

I submitted two more transactions to make a total of three - two with the correct datum and one with the wrong datum.

.. note::

    When running subsequent transactions, you may be presented with the option to use the same UTxO that was used for a previous transaction. This won't be allowed, so you need
    to choose a different UTxO, or, if none exist with enough funds, you need to wait for the previous transaction to complete, by which time a new UTxO (the change from the previous transaction)
    will be available.

.. code:: bash

    ./payToScript.sh 72500000 HelloWorldPerson "\"Sam Jones\"" wallet1
    ./payToScript.sh 72500000 HelloWorldPerson "\"Sammy Jones\"" wallet1

.. code:: bash

    ./contractBalance.sh HelloWorldPerson

    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    48b33ea5694c8b7d65384eb67470bdc28202d7fe211a60045d0b667c795a22b6     0        62500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "b3c689968968928e5b87c4a74675b85f311c475a011ec2f168261ce0ae85774a"
    ce0b7f4978b7cd6dae6946a1e150964908491583cacb9436085ac52975ee56c8     0        72500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "b3c689968968928e5b87c4a74675b85f311c475a011ec2f168261ce0ae85774a"
    e81da06411acf518cd3e988de27455db757ad5dcdd39bf403bdc1c173880593d     0        72500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "7cfec515f56d4413375aa9775f5de15ee60180861e9eaa954bcf9d015054857c"

Now we have to put some ugly strings on the command line because I'm not very good with *bash*. The third argument is the redeemer.

.. code:: bash

    ./getFromScript.sh HelloWorldPerson "\"Sam Jones\"" "\"1974/12/23\"" wallet1

    ============================================================================================
    Select Script UTxO
    ============================================================================================
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    48b33ea5694c8b7d65384eb67470bdc28202d7fe211a60045d0b667c795a22b6     0        62500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "b3c689968968928e5b87c4a74675b85f311c475a011ec2f168261ce0ae85774a"
    ce0b7f4978b7cd6dae6946a1e150964908491583cacb9436085ac52975ee56c8     0        72500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "b3c689968968928e5b87c4a74675b85f311c475a011ec2f168261ce0ae85774a"
    e81da06411acf518cd3e988de27455db757ad5dcdd39bf403bdc1c173880593d     0        72500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "7cfec515f56d4413375aa9775f5de15ee60180861e9eaa954bcf9d015054857c"
    TX row number: 2
    ============================================================================================
    Select Collateral UTxO
    ============================================================================================
    Wallet Name: fees
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    099a2a3d025d4e30e95410be19d67e3a27b6c237b378ac8e3f89806d7d1922a7     0        889798683 lovelace + TxOutDatumHashNone
    TX row number: 1
    Receiving Wallet: wallet2
    Transaction successfully submitted.

After waiting a minute, we check that we managed to grab the funds.

.. code:: bash

    ./contractBalance.sh HelloWorldPerson
    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    48b33ea5694c8b7d65384eb67470bdc28202d7fe211a60045d0b667c795a22b6     0        62500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "b3c689968968928e5b87c4a74675b85f311c475a011ec2f168261ce0ae85774a"
    e81da06411acf518cd3e988de27455db757ad5dcdd39bf403bdc1c173880593d     0        72500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "7cfec515f56d4413375aa9775f5de15ee60180861e9eaa954bcf9d015054857c"

    ./balance.sh wallet1
    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    060aa2af10655a4b893bb4b828aa2288a5a18f1dd8941a7f99ffe8d3bd1d71f1     1        99000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"
    11248966667145ab998933075d4eca6305e96b58e03684c7d1c8100a410c17df     1        62500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "8fb8d1694f8180e8a59f23cce7a70abf0b3a92122565702529ff39baf01f87f1"
    776df51578af3b840edebf1cb7d0466ce7c60e5230ffe461cf017e2c631c3de7     0        99498320857 lovelace + TxOutDatumHashNone
    8fffaa8ca32ff2af2e984a85ece19efaf5aebac5527e0954ca009cb54954acc4     1        72500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "b3c689968968928e5b87c4a74675b85f311c475a011ec2f168261ce0ae85774a"


If you pass an incorrect datum, you will receive an error message and the transaction will not enter validation.

.. code:: bash

    ./getFromScript.sh HelloWorldPerson "\"Sammy Jones\"" "\"1974/12/23\"" wallet1
    ...
    ...
    Command failed: transaction submit  Error: Error while submitting tx: ShelleyTxValidationError ShelleyBasedEraAlonzo (ApplyTxError [UtxowFailure (MissingRequiredDatums (fromList [SafeHash "b3c689968968928e5b87c4a74675b85f311c475a011ec2f168261ce0ae85774a"]) (fromList [SafeHash "d658ccd4fce5643c6186657cc2f88f2d110acb88c8b94cd90d9acb088562a19a"]))])

