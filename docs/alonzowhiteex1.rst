Alonzo White Walkthrough
========================

These are my notes taken while working through the Alonzo White exercises.

This is the setup I chose to use, but there are several other ways to run the node and configure the environment.

.. note::
    
    The exercises are only possible if you have some test Ada from the Alonzo White faucet. Test Ada is currently only available to members
    of the Alonzo test group in order to control usage of the network. Alonzo testnet is moving towards a public phase within a matter of weeks.

I started with a fresh ``t2.large`` AWS Ubuntu EC2 instance using AMI ``ami-0ff4c8fb495a5a50d`` and adding a 20Gb data volume.

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
    echo "export NIX_IGNORE_SYMLINK_STORE=1" >> "~/.bashrc
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

.. code:: json

    {
        "epoch": 289,
        "hash": "7521f071d0bfc050cde302f1352ed44c2fc74927f1e28afea1b1df2c4c012d5c",
        "slot": 2079664,
        "block": 102026,
        "era": "Alonzo",
        "syncProgress": "100.00"
    }

You can use ``jq`` if you ever want to get some specific information, for example:

.. code:: bash

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

Then, check that it has arrived.

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

