Alonzo White Walkthrough
========================

These are my notes taken while working through the Alonzo White exercises.

The notes take you through the process of setting up an AWS instance running an Alonzo White node, and using the command line to 
move ADA between wallets and submit and executing smart contracts. It uses several helper scripts to simplify the creation of the commands. The helper scripts are
simple bash scripts that create ``cardano-cli`` commands and should be easy to understand to get a grip on what is going on behind the scenes.

This is the setup I chose to use, but there are several other ways to run the node and configure the environment.

.. note::
    
    The exercises are only possible if you have some test Ada from the Alonzo White faucet. Test Ada is currently only available to members
    of the Alonzo test group in order to control usage of the network. Alonzo testnet is moving towards a public phase within a matter of weeks.

I started with a fresh ``t2.large`` AWS Ubuntu EC2 instance using AMI ``ami-0ff4c8fb495a5a50d`` and adding a 60Gb data volume.

AWS Node Setup
--------------

Mount the data volume.

.. code:: bash

    sudo mkfs -t xfs /dev/xvdh
    sudo mkdir /data
    sudo mount /dev/xvdh /data
    sudo chown ubuntu:ubuntu /data

Setup the IOHK Cache
____________________

.. code:: bash

    sudo mkdir -p /etc/nix
    cat <<EOF | sudo tee /etc/nix/nix.conf
    substituters = https://cache.nixos.org https://hydra.iohk.io https://iohk.cachix.org
    trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo=
    EOF

Install Nix
___________

We use a little trick here to let Nix use a symlinked directory. This is not recommended if you plan to have setups on multiple machines with potentially different configurations,
but that doesn't matter here.

.. code:: bash

    mkdir /data/nix
    sudo ln -s /data/nix /nix
    echo "export NIX_IGNORE_SYMLINK_STORE=1" >> ~/.bashrc
    source ~/.bashrc
    curl -L https://nixos.org/nix/install | sh
    . /home/ubuntu/.nix-profile/etc/profile.d/nix.sh

Download the Cardano Node
_________________________

.. code:: bash

    cd /data
    git clone https://github.com/input-output-hk/cardano-node
    cd cardano-node
    git checkout tags/alonzo-white-1.1 -b alonzo_white_1_1

Build the node
______________

.. code:: bash

    nix-build -A scripts.alonzo-white.node -o result/alonzo-white/cardano-node-alonzo-white
    nix-build -A cardano-cli -o result/alonzo-white/cardano-cli

Start the node
______________

.. code:: bash

    cd /data/cardano-node/result/alonzo-white
    ./cardano-node-alonzo-white/bin/cardano-node-alonzo-white

Leave this running and open another shell.

Setup some environment variables
________________________________

.. code:: bash

    echo "export CARDANO_CLI=/data/cardano-node/result/alonzo-white/cardano-cli/bin/cardano-cli" >> ~/.bashrc
    echo "export TESTNET_MAGIC_NUM=7" >> ~/.bashrc
    echo "export CARDANO_NODE_SOCKET_PATH=/data/cardano-node/result/alonzo-white/state-node-alonzo-white/node.socket" >> ~/.bashrc
    source ~/.bashrc

You can check on the status of the node with:

.. code:: bash

    $CARDANO_CLI query tip --testnet-magic $TESTNET_MAGIC_NUM

You should see something like this:

.. code:: bash

    {
        "epoch": 60,
        "hash": "eb9453a91760928b286ea5137d6f9325f89f78b9c643f1e789c63c74b1934fa3",
        "slot": 431693,
        "block": 21187,
        "era": "Mary",
        "syncProgress": "19.01"
    }

When the node has fully synced you will see that the era has changed to ``Alonzo``.

.. code:: json

    {
        "epoch": 289,
        "hash": "7521f071d0bfc050cde302f1352ed44c2fc74927f1e28afea1b1df2c4c012d5c",
        "slot": 2079664,
        "block": 102026,
        "era": "Alonzo",
        "syncProgress": "100.00"
    }

You can use ``jq`` if you ever want to get some specific information on its own, for example:

.. code:: bash

    sudo apt update
    sudo apt install jq -y
    
    $CARDANO_CLI query tip --testnet-magic 7 | jq -r '.syncProgress'
    100.0

Some Helper Scripts
-------------------

I have a repo that contains a few helper scripts that I use. It's rough and ready, but saves a little time for some common tasks.

.. code:: bash
    
    cd /data
    git clone https://github.com/chris-moreton/plutus-scripts

Getting Test Ada
----------------

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

AlwaysSucceeds Script
---------------------

Plutus scripts get compiled down to the following format. 

.. code:: json

    {
        "type": "PlutusScriptV1",
        "description": "",
        "cborHex": "585c585a010000332233322233333322222233222220051200120012122222300500622122222330040070062122222300300621222223002006212222230010062001112200212212233001004003120011122123300100300211200101"
    }   
    
This is the ``AlwaysSuccess.plutus`` script whose validator always succeeds, which means that anyone will be able to consume any UTxO sitting at its address. The script can be found in the ``/data/plutus-scripts/scripts`` directory. 

Pay to the Script
_________________

Using the ``payToScript.sh`` helper script, you can send ``99000000`` lovelace from ``wallet`` to the ``AlwaysSucceeds.plutus`` script with a datum of ``6666``, allowing for fees of ``200000``.

.. code:: bash

    ./payToScript.sh 99000000 200000 AlwaysSucceeds 6666 wallet1

    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    bd7422ef2cd55d1c5a33601a3b75b080bc3742856e5ddb8dfdfae07f583c7af1     0        1000000000 lovelace + TxOutDatumHashNone
    TX row number: 1
    Transaction successfully submitted.

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
______________________________

When trying to consume UTxOs locked in a script, you need to provide collateral that will cover the costs if validation fails. For this we can use a separate wallet for storing
collateral UTxOs, to keep things tidy. One of the several wallets we created earlier on was named ``fees``.

Under normal circumstances, collateral should never be lost because the wallet can perform validation in a deterministic fashion and only
submit the transaction if validation is guaranteed to pass.

.. code:: bash

    ./sendFromWallet.sh main

                            TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    bd7422ef2cd55d1c5a33601a3b75b080bc3742856e5ddb8dfdfae07f583c7af1     1        998999800000 lovelace + TxOutDatumHashNone
    TX row number: 1  
    Lovelace to send: 1000000000
    Receiving wallet name: fees
    Transaction successfully submitted.

We should check that it's arrived in our ``fees`` wallet.

.. code:: bash

    ./balance.sh fees
                            TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    7678d8d6b95ed026d7c690fb53419bdaa580cb00c56450ac3bd97712dd71ca4e     0        1000000000 lovelace + TxOutDatumHashNone

The following command will try to get 1000000 lovelace from the script using fees of ``100000000`` and a datum of ``6666``, which is the correct datum.

.. code:: bash

    ./getFromScript.sh 1000000 100000000 AlwaysSucceeds 6666
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
    Receiving Wallet: wallet2

    Command failed: transaction submit  Error: Error while submitting tx: ShelleyTxValidationError ShelleyBasedEraAlonzo (ApplyTxError [UtxowFailure (WrappedShelleyEraFailure (UtxoFailure (FeeTooSmallUTxO (Coin 110180197) (Coin 100000000))))])
    
Here the transaction has failed because the fees were too low. It tells us what the fees should be, so we can try again with.

.. code:: bash

    ./getFromScript.sh 1000000 110180197 AlwaysSucceeds 6666
    ...
    Transaction successfully submitted.

Let's check that it arrived in ``wallet2`` as expected.

.. code:: bash

    ./balance.sh wallet2

    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    ee22529028220bb2d2cbda634fbe982602afd5baf7f173341e2c8f9157e2912d     1        1000000 lovelace + TxOutDatumHashNone

We have managed to extract 1,000,000 lovelace from the contract.

Let's try it with an invalid datum.

.. code:: bash
    
    ./getFromScript.sh 1000000 110180197 AlwaysSucceeds 5555
    ...
    Command failed: transaction submit  Error: Error while submitting tx: ShelleyTxValidationError ShelleyBasedEraAlonzo (ApplyTxError [UtxowFailure (MissingRequiredDatums (fromList [SafeHash "9e1199a988ba72ffd6e9c269cadb3b53b5f360ff99f112d9b2ee30c4d74ad88b"]) (fromList [SafeHash "71f5a96d948593ef12667c22d49b5dbbed7f00c7a3e88083cdf7391c5cc3ba73"]))])    

AlwaysFails Script
------------------

Now we will try similar transactions with a script whose validator always fails. This time we will lose our collateral.

First, lock some lovelace in the AlwaysFails script which will be picked up from the ``./scripts`` directory of the cloned ``plutus-scripts`` repository.

.. code:: bash

    ./payToScript.sh 99000000 200000 AlwaysFails 6666 wallet1

    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    843f4ffa4aafc5ed968d0a9f0fb8a203796b66327343246bfd8d4ca1d361c2f8     1        900800000 lovelace + TxOutDatumHashNone
    TX row number: 1
    Transaction successfully submitted.
    
Check that it has arrived.

.. code:: bash

    ./contractBalance.sh AlwaysFails

    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    0913d72c55b3d6e765eb51f1a5da1436ea0554a89d499fe3398d20517f0b455e     0        99000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"
    29f82f40603c4328e6efffb7c6e8851fe9540d18ddc0930b188896f6b016e141     0        100000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "ee5c9e2778c6c398366c5b9cfd67a888081f7626ca0ac392faca5981e59ff759"
    5589e823cf148597cbf64dc7cb5ebcd3957d5fc83c3521b281daa9f9c490c8ab     0        999888777 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"

Now we try to get some funds from the script.

.. code:: bash

    ./getFromScript.sh 1000000 110180417 AlwaysFails 6666
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
    Receiving Wallet: wallet2
    Transaction successfully submitted.

But will we now find that the collateral has gone. The validation failed.

.. code:: bash

    ./balance.sh fees
    TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------

And the funds will never arrive. We still have just one UTxO, the one we got from the ``AlwaysSucceeds`` script.

.. code:: bash

    ./balance.sh wallet2
                           TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    ee22529028220bb2d2cbda634fbe982602afd5baf7f173341e2c8f9157e2912d     1        1000000 lovelace + TxOutDatumHashNone


HelloWorld Script
-----------------

Compiling Plutus Scripts
________________________

First, we need to go into a Nix shell. The shell from ``cardano-node`` doesn't work for me, so I cloned the ``plutus`` repository and used its shell instead.

.. code:: bash

    cd /data
    git clone https://github.com/input-output-hk/plutus
    cd plutus
    nix-shell

Now, we can change to the ``plutus-helloworld`` directory and run the code.

.. code:: bash

    cd /data/plutus-scripts/plutus-sources/plutus-helloworld

If you look in the ``plutus-helloworld.cabal`` file in, you'll find some executables defined, one of which is called ``plutus-helloworld``. 

Running this executable will create compiled Plutus code as output.

.. code:: bash

    cabal update
    cabal run plutus-helloworld

This outputs, among other things, something like the following.

.. code:: bash

    ExBudget {_exBudgetCPU = ExCPU 9814000, _exBudgetMemory = ExMemory 2260}

These value represent the expected CPU and memory usage required to run the script. Fees for running scripts are calculated using these values.

It also outputs a file called ``result.plutus``, which contains the compiled Plutus code.

.. code:: json

    {
        "type": "PlutusScriptV1",
        "description": "",
        "cborHex": "5902355902320100003232323332223322333222323232323322333333222222323232323232222333530163335006300812001003300412001201b23500601b01a2300a4830af38f1ab6649049848004d400888004d400488008d55540584888cc00c888ccccd4c01c0088cccccd4c02000888cc04c00c0088880608805c8805c8805c06088cccccd4c02400c88060888ccd4c060cc05001400c8ccd4c05c48004c02c4800401400c80680708806088060880600648cccccd4c0200088805c88806088cc04800c0088805c8805c0608cccccd4c0200088805c8880608805c88ccd4c05048004c0204800400c0088805c0608cccccd4c0200088805c8880608805c8805c88ccd4c0504800401c00c00806088d4d5403000888d4d5403800c88ccd4c058ccc020480040100088ccc0244800401000880600688488888c01401888488888cc01001c0188488888c00c0188488888c0080188488888c0040188004448848cc00400c00844800488c8ccd5ea00201201066e9002001223233357a80080400399bc0008004d4004800448c004d54028884888ccd4d40300088ccd4d4034008802c888030030888ccd4d403c0108030888ccd4c034cc02401800c8ccd4c0304800402801800c803c04403802c48800848800480044800480044488008488488cc00401000c48004448c8c00400488cc00cc8c00400400800444448cd4008848cc0048d400c88c00800c8d400c88c00400c00448c8c8c00400488cc00cc8c0040040080048848d4c01048d4c01000cd400c0040041"
    }

We can now move this script to our ``scripts`` directory and give it a more useful name.

.. code:: bash

    mv result.plutus ../../scripts/HelloWorld.plutus
    



