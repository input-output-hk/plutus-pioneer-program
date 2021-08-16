HelloWorld Script
=================

.. include:: ../notes/setup.rst

Compiling Plutus Scripts
________________________

First, we need to go into a Nix shell. The shell from ``cardano-node`` doesn't work for me, so I cloned the ``plutus`` repository and used its shell instead.

.. code:: bash

    cd /data
    git clone https://github.com/input-output-hk/plutus
    cd plutus
    nix-shell

Now we will clone the ``Alonzo-testnet`` repo.

.. code:: bash

    cd /data
    git clone https://github.com/input-output-hk/Alonzo-testnet

Now, we can change to the ``plutus-helloworld`` directory and run the code.

.. code:: bash

    cd /data/Alonzo-testnet/resources/plutus-sources/plutus-helloworld

If you look in the ``plutus-helloworld.cabal`` file in, you'll find some executables defined, one of which is called ``plutus-helloworld``. 

Running this executable will create compiled Plutus code as output.

.. code:: bash

    cabal update
    cabal run plutus-helloworld

This outputs, among other things, something like the following.

.. code:: bash

    ExBudget {_exBudgetCPU = ExCPU 2443000, _exBudgetMemory = ExMemory 370}

These value represent the expected CPU and memory usage required to run the script. Fees for running scripts are calculated using these values.

It also outputs a file called ``result.plutus``, which contains the compiled Plutus code.

.. code:: json

    {
        "type": "PlutusScriptV1",
        "description": "",
        "cborHex": "585e585c01000033322232323233223232322223335300c333500b00a0033004120012009235007009008230024830af38f1ab664908dd3800891a8021a9801000a4c24002400224c44666ae54cdd70010008030028900089100109100090009"
    }

We can now move this script to our ``scripts`` directory and give it a more useful name.

.. code:: bash

    mv result.plutus /data/plutus-scripts/scripts/HelloWorld.plutus

Now we will lock some funds in the script. We will use the datum ``79600447942433``. You will see from the comments in the ``HelloWorld.hs`` file that this is the ``hello world``
message converted to an Integer and shortened to fit within the 8-byte limit for an ``int`` datum.

.. code:: bash

    cd /data/plutus-scripts

    ./payToScript.sh 62500000 HelloWorld 79600447942433 wallet1
    Wallet Name:  wallet1
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    060aa2af10655a4b893bb4b828aa2288a5a18f1dd8941a7f99ffe8d3bd1d71f1     1        99000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"
    ae892df5c983f98dff69e7a748d8d8609f823e7a67c9d2a8834d9bd1927a91ae     0        99801663102 lovelace + TxOutDatumHashNone
    TX row number: 2
    Transaction successfully submitted.      

The ``hello world`` value is defined in the script as

.. code:: haskell

    hello :: Data
    hello = I 0x48656c6c6f21
    
And the validator will check that the datum matches this value. Any UTxO at this script address with a different datum will not be spendable.

.. code:: haskell

    helloWorld :: Data -> Data -> Data -> ()
    helloWorld datum redeemer context = if datum P.== hello then () else (P.error ())

So, let's lock some more lovelace in the script but with a different datum.

.. code:: bash

    ./payToScript.sh 32500000 HelloWorld 89600447942433 wallet1
    Wallet Name: wallet1
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    ca54c8370065e3d385720b9f863d115a6ffb54dfc5b517965cd1a9b02bd34ac9     1        665000000 lovelace + TxOutDatumHashNone
    TX row number: 1
    Transaction successfully submitted.

Let's look at the UTxOs locked in the script.

.. code:: bash

    ./contractBalance.sh HelloWorld

We see a lot of them, including our first one which has a datum hash of ``8fb8d1694f8180e8a59f23cce7a70abf0b3a92122565702529ff39baf01f87f1``. We know this is the hash of the correct datum, so
we should be able to spend this UTxO. We don't see any others with this datum hash, which means that those ones are locked forever.

.. code:: bash

    ./contractBalance.sh HelloWorld
                            TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    0dfec1295895d877edf15f323df63f43aa4501bfc8ee0483512c13550b6f4a65     0        98000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "ee5c9e2778c6c398366c5b9cfd67a888081f7626ca0ac392faca5981e59ff759"
    867226273c7de3bbeb9e94f2451bafc10f20a66d3018142e87490349c92b9db0     0        32500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "ea7db071348c44ee3ba423fbf61c57edc91167f78835d037c5c7503ed1a5fa5d"
    325704fa84cc1bfbbf69688a21d66157ddc7145be92567b6068d31de31bbb33c     0        1000000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "74fa514fdca080be51ea4fce15f6033b754c5dc3455cb9db8dfd930623a2b4bb"
    5ae97b8af41817ed4866360d10b48d9a535421fe3ee3497a09e6f4fe2d44251e     0        100000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "52d800d39486d8234e08050de3fa06296497a3a44343b4494801eb502ce38f93"
    71c1fcf524dbd24be33e27ed9a0f9e3a8648609401d47c41c66299573052dbbd     0        6666666 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"
    79fc6f53b741f2dacb6f46ee723a99b4590566f71eaa970f2c859539a2621fbf     0        98000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "c22bc0debed6579418c5a07e591e5623eac7f5bf0b0d1906e7b35c7adfe66e7d"
    978681fa039c391bf37b1f2ba57312a10e2b823cf68c01fb627973b36064e452     1        512000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "4250ea713ad7ba3b121621a8d14d8e39a4300065314b7ce9a40526acf992c8e3"
    a05563264c201047514185b38f6af833cc62122074aba5d217506b2be4f5955e     0        62500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "8fb8d1694f8180e8a59f23cce7a70abf0b3a92122565702529ff39baf01f87f1"
    a9b481492d5633dbf03cc92ea59e1c8f5b5c1b1f63abe9b3915a4cabf716602e     0        1000000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "7cfec515f56d4413375aa9775f5de15ee60180861e9eaa954bcf9d015054857c"
    bede7e2a28c9bdbe5e52752756c30b93ac7523cacdf42ad474fdc04ec750dee5     0        50000000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "b26b5f203ca94dda3d333621ea493f7ca26fef90fd1cc5fa678b38737371cb79"
    dd58557b3e780703126a60e340fb13a681b47793771e5980dbb0ef479c905db6     0        1000000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "ff92136db7aec02e06f0d93ed5cbea5d33360061e5d3cab51a827d65fdeb33ad"
    e97ca5246c9b1565250e2cc5078d770564463a57c13125a37e4906c1f7dc0680     0        50000000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9ad30ffde0d1931ed4f145fa0a0d320a067051bfab1b08cbdb79e9f26df55df3"
    f6179d20172fec17caae32791623da52e2aa3bff04304389f693672aa1e3dae3     0        100000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"

We will try to get some funds from the one with the ``89600447942433`` datum. 

If we pass in a datum that is not ``89600447942433``, we'll get the following error. The ``42`` here is a redeemer value that is ignored in this case.

.. code:: bash

    ./getFromScript.sh HelloWorld 69600447942433 42 wallet1
    ...
    ...
    Command failed: transaction build  Error: The following scripts have execution failures: the script for transaction input 0 (in the order of the TxIds) failed with The Plutus script witness has the wrong datum (according to the UTxO). The expected datum value has hash "8fb8d1694f8180e8a59f23cce7a70abf0b3a92122565702529ff39baf01f87f1"
    Command failed: transaction submit  Error: Error while submitting tx: ShelleyTxValidationError ShelleyBasedEraAlonzo (ApplyTxError [UtxowFailure (WrappedShelleyEraFailure (UtxoFailure (ValueNotConservedUTxO (Value 0 (fromList [])) (Value 9738994653 (fromList []))))),UtxowFailure (WrappedShelleyEraFailure (UtxoFailure (BadInputsUTxO (fromList [TxInCompact (TxId {_unTxId = SafeHash "8ae6249f7df83f3f465e843c419985a816f27f0ff8ab5214ae7dc68c20d52da7"}) 0]))))])

So, we'll pass in the matching datum.

.. code:: bash

    ./getFromScript.sh HelloWorld 89600447942433 42 wallet1

    ============================================================================================
    Select Script UTxO
    ============================================================================================
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    0dfec1295895d877edf15f323df63f43aa4501bfc8ee0483512c13550b6f4a65     0        98000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "ee5c9e2778c6c398366c5b9cfd67a888081f7626ca0ac392faca5981e59ff759"
    2d9400af7637b05b34e96c66781f087c7a28c7da8b4482b98897807dfe84efd6     0        32500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "3519b7fbee1f70218539524e3b50ba8fa67b6d769cfa6fee4d4356e800342956"
    ...
    a05563264c201047514185b38f6af833cc62122074aba5d217506b2be4f5955e     0        62500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "8fb8d1694f8180e8a59f23cce7a70abf0b3a92122565702529ff39baf01f87f1"

    TX row number: 2
    ============================================================================================
    Select Collateral UTxO
    ============================================================================================
    Wallet Name: fees
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    65e74914ad63e7e3372da140a3285b3d7eea879d02d6b814cf1f23df40c44418     0        1000000000 lovelace + TxOutDatumHashNone
    c5b11e878a7dcebe5a52eb32eff5d83c3c76e35d13a2106aab811535dff5e3f6     0        1000000000 lovelace + TxOutDatumHashNone
    TX row number: 1
    Receiving Wallet: wallet2

    Command failed: transaction submit  Error: Error while submitting tx: ShelleyTxValidationError ShelleyBasedEraAlonzo (ApplyTxError [UtxowFailure (WrappedShelleyEraFailure (UtxoFailure (UtxosFailure (ValidationTagMismatch (IsValid True)))))])

This time, the datums match, but the value of the datum is incorrect, and we fail validation and so do not submit the transaction to the blockchain. 

So, let's try to get some funds from the UTxO with the ``hello world`` message as a datum. The validator script will let us unlock that one.

.. code:: bash

    ./getFromScript.sh HelloWorld 79600447942433 42 wallet1
    ============================================================================================
    Select Script UTxO
    ============================================================================================
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    0dfec1295895d877edf15f323df63f43aa4501bfc8ee0483512c13550b6f4a65     0        98000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "ee5c9e2778c6c398366c5b9cfd67a888081f7626ca0ac392faca5981e59ff759"
    2d9400af7637b05b34e96c66781f087c7a28c7da8b4482b98897807dfe84efd6     0        32500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "3519b7fbee1f70218539524e3b50ba8fa67b6d769cfa6fee4d4356e800342956"
    325704fa84cc1bfbbf69688a21d66157ddc7145be92567b6068d31de31bbb33c     0        1000000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "74fa514fdca080be51ea4fce15f6033b754c5dc3455cb9db8dfd930623a2b4bb"
    5ae97b8af41817ed4866360d10b48d9a535421fe3ee3497a09e6f4fe2d44251e     0        100000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "52d800d39486d8234e08050de3fa06296497a3a44343b4494801eb502ce38f93"
    71c1fcf524dbd24be33e27ed9a0f9e3a8648609401d47c41c66299573052dbbd     0        6666666 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"
    79fc6f53b741f2dacb6f46ee723a99b4590566f71eaa970f2c859539a2621fbf     0        98000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "c22bc0debed6579418c5a07e591e5623eac7f5bf0b0d1906e7b35c7adfe66e7d"
    978681fa039c391bf37b1f2ba57312a10e2b823cf68c01fb627973b36064e452     1        512000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "4250ea713ad7ba3b121621a8d14d8e39a4300065314b7ce9a40526acf992c8e3"
    a05563264c201047514185b38f6af833cc62122074aba5d217506b2be4f5955e     0        62500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "8fb8d1694f8180e8a59f23cce7a70abf0b3a92122565702529ff39baf01f87f1"

    TX row number: 8
    ============================================================================================
    Select Collateral UTxO
    ============================================================================================
    Wallet Name: fees
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    c5b11e878a7dcebe5a52eb32eff5d83c3c76e35d13a2106aab811535dff5e3f6     0        1000000000 lovelace + TxOutDatumHashNone

    TX row number: 1
    Transaction successfully submitted.

Now, after a minute or so, when we look at the UTxOs in ``wallet1``, we see that we have got some new lovelace.

.. code:: bash

    ./balance.sh wallet1
    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    060aa2af10655a4b893bb4b828aa2288a5a18f1dd8941a7f99ffe8d3bd1d71f1     1        99000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"
    11248966667145ab998933075d4eca6305e96b58e03684c7d1c8100a410c17df     1        62500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "8fb8d1694f8180e8a59f23cce7a70abf0b3a92122565702529ff39baf01f87f1"
    7760e20988b77005a557e937e493aaf98103ec46e6b9ddc90ed27485bf8602d0     0        99706326204 lovelace + TxOutDatumHashNone

We did not lose our collateral, although we did get charged some fees.

.. code:: bash

    ./balance.sh fees
    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    11248966667145ab998933075d4eca6305e96b58e03684c7d1c8100a410c17df     0        99999577494 lovelace + TxOutDatumHashNone

And the UTxO we spent has gone, but has been replaced with a new UTxO with a reduced balance, but the same datum, leaving us free to take more funds from it later.

.. code:: bash

    ./contractBalance.sh HelloWorld
        TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    099a2a3d025d4e30e95410be19d67e3a27b6c237b378ac8e3f89806d7d1922a7     2        42500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "8fb8d1694f8180e8a59f23cce7a70abf0b3a92122565702529ff39baf01f87f1"
    0dfec1295895d877edf15f323df63f43aa4501bfc8ee0483512c13550b6f4a65     0        98000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "ee5c9e2778c6c398366c5b9cfd67a888081f7626ca0ac392faca5981e59ff759"
    2d9400af7637b05b34e96c66781f087c7a28c7da8b4482b98897807dfe84efd6     0        32500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "3519b7fbee1f70218539524e3b50ba8fa67b6d769cfa6fee4d4356e800342956"
    ...



    