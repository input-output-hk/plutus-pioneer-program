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

