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

    ./payToScript.sh 62500000 200000 HelloWorldPerson "\"Sam Jones\""
    Wallet Name: wallet1
                            TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    e81da06411acf518cd3e988de27455db757ad5dcdd39bf403bdc1c173880593d     1        569600000 lovelace + TxOutDatumHashNone
    TX row number: 1
    Transaction successfully submitted.

I submitted three transactions, two with the correct datum and one without.

.. code:: bash

    ./contractBalance.sh HelloWorldPerson

    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    48b33ea5694c8b7d65384eb67470bdc28202d7fe211a60045d0b667c795a22b6     0        62500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "b3c689968968928e5b87c4a74675b85f311c475a011ec2f168261ce0ae85774a"
    ce0b7f4978b7cd6dae6946a1e150964908491583cacb9436085ac52975ee56c8     0        12500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "b3c689968968928e5b87c4a74675b85f311c475a011ec2f168261ce0ae85774a"
    e81da06411acf518cd3e988de27455db757ad5dcdd39bf403bdc1c173880593d     0        62500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "7cfec515f56d4413375aa9775f5de15ee60180861e9eaa954bcf9d015054857c"

Now we have to put some ugly strings on the command line because I'm not very good with *bash*. The final argument is the redeemer.

.. code:: bash

    ./getFromScript.sh 6500000 120000000 HelloWorldPerson "\"Sam Jones\"" "\"1974/12/23\""

    ============================================================================================
    Select Script UTxO
    ============================================================================================
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    48b33ea5694c8b7d65384eb67470bdc28202d7fe211a60045d0b667c795a22b6     0        62500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "b3c689968968928e5b87c4a74675b85f311c475a011ec2f168261ce0ae85774a"
    ce0b7f4978b7cd6dae6946a1e150964908491583cacb9436085ac52975ee56c8     0        12500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "b3c689968968928e5b87c4a74675b85f311c475a011ec2f168261ce0ae85774a"
    e81da06411acf518cd3e988de27455db757ad5dcdd39bf403bdc1c173880593d     0        62500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "7cfec515f56d4413375aa9775f5de15ee60180861e9eaa954bcf9d015054857c"
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

After waiting a minute, we check that we managed to grab some funds.

.. code:: bash

    ./contractBalance.sh HelloWorldPerson
    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    48b33ea5694c8b7d65384eb67470bdc28202d7fe211a60045d0b667c795a22b6     0        62500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "b3c689968968928e5b87c4a74675b85f311c475a011ec2f168261ce0ae85774a"
    6d5b5c760fdf83af12fd071e4d89b7058afb42be98ed8557cc3fe30047047a2f     2        6000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "b3c689968968928e5b87c4a74675b85f311c475a011ec2f168261ce0ae85774a"
    e81da06411acf518cd3e988de27455db757ad5dcdd39bf403bdc1c173880593d     0        62500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "7cfec515f56d4413375aa9775f5de15ee60180861e9eaa954bcf9d015054857c"

    ./balance.sh wallet2
    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    099a2a3d025d4e30e95410be19d67e3a27b6c237b378ac8e3f89806d7d1922a7     1        20000000 lovelace + TxOutDatumHashNone
    36a1072bd69c6f7307fdb017e796ccd0fdd953a21dc9fb34bf015fad1cb1560c     1        1000000 lovelace + TxOutDatumHashNone
    6d5b5c760fdf83af12fd071e4d89b7058afb42be98ed8557cc3fe30047047a2f     1        6500000 lovelace + TxOutDatumHashNone

If you pass an incorrect datum, you will receive an error message and the transaction will not enter validation.

.. code:: bash

    ./getFromScript.sh 1500000 120000000 HelloWorldPerson "\"Sammy Jones\"" "\"1974/12/23\""
    ...
    Command failed: transaction submit  Error: Error while submitting tx: ShelleyTxValidationError ShelleyBasedEraAlonzo (ApplyTxError [UtxowFailure (MissingRequiredDatums (fromList [SafeHash "b3c689968968928e5b87c4a74675b85f311c475a011ec2f168261ce0ae85774a"]) (fromList [SafeHash "d658ccd4fce5643c6186657cc2f88f2d110acb88c8b94cd90d9acb088562a19a"]))])

If you pass the correct datum but an incorrect redeemer, then you will lose your collateral.

.. code:: bash

    ./getFromScript.sh 1500000 120000000 HelloWorldPerson "\"Sam Jones\"" "\"1975/12/23\""
