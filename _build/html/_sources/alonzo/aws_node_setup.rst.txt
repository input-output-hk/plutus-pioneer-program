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
