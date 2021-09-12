Install the Cardano node
========================

I started with two fresh ``t2.large`` AWS Ubuntu EC2 instances using AMI ``ami-0ff4c8fb495a5a50d``, each with a 60Gb data volume.

Perform these steps on each of the instances.

Mount the data volume
---------------------

.. code:: bash

    sudo mkfs -t xfs /dev/xvdh
    sudo mkdir /data
    sudo mount /dev/xvdh /data
    sudo chown ubuntu:ubuntu /data

Setup some environment variables
--------------------------------

.. code:: bash

    echo "export PATH=\"~/.local/bin:$PATH\"" >> ~/.bashrc
    echo "export LD_LIBRARY_PATH=\"/usr/local/lib:\$LD_LIBRARY_PATH\"" >> ~/.bashrc
    echo "export PKG_CONFIG_PATH=\"/usr/local/lib/pkgconfig:\$PKG_CONFIG_PATH\"" >> ~/.bashrc
    echo "export CARDANO_NODE_SOCKET_PATH=\"/data/Pool/node/db.socket\"" >> ~/.bashrc
    echo "export CABAL_VERSION=3.2.0.0" >> ~/.bashrc
    echo "export CARDANO_TAG=1.29.0" >> ~/.bashrc
    echo "export GHC_VERSION=8.10.2" >> ~/.bashrc

    source ~/.bashrc

Install some dependencies
-------------------------

.. code:: bash

    sudo apt-get update -y
    sudo apt-get install automake build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libncursesw5 libtool autoconf -y

Install Cabal
-------------

.. code:: bash

    mkdir /data/Downloads
    cd /data/Downloads

    wget https://downloads.haskell.org/~cabal/cabal-install-$CABAL_VERSION/cabal-install-$CABAL_VERSION-x86_64-unknown-linux.tar.xz
    tar -xf cabal-install-$CABAL_VERSION-x86_64-unknown-linux.tar.xz
    rm cabal-install-$CABAL_VERSION-x86_64-unknown-linux.tar.xz cabal.sig
    
    mkdir -p ~/.local/bin
    mv cabal ~/.local/bin/
    cabal update

Install GHC
-----------

.. code:: bash

    wget https://downloads.haskell.org/ghc/$GHC_VERSION/ghc-$GHC_VERSION-x86_64-deb9-linux.tar.xz
    tar -xf ghc-$GHC_VERSION-x86_64-deb9-linux.tar.xz
    rm ghc-$GHC_VERSION-x86_64-deb9-linux.tar.xz
    cd ghc-$GHC_VERSION
    ./configure
    sudo make install

Install libsodium
-----------------

.. code:: bash

    cd /data/Downloads
    git clone https://github.com/input-output-hk/libsodium
    cd libsodium
    git checkout 66f017f1
    
    ./autogen.sh
    ./configure
    make
    sudo make install

Build the Cardano node
----------------------

.. code:: bash

    cd /data
    git clone https://github.com/input-output-hk/cardano-node.git
    cd cardano-node
    git fetch --all --tags
    git checkout tags/$CARDANO_TAG
    cabal build all

Copy the binaries
-----------------

.. code:: bash

    cp -p dist-newstyle/build/x86_64-linux/ghc-$GHC_VERSION/cardano-node-$CARDANO_TAG/x/cardano-node/build/cardano-node/cardano-node ~/.local/bin/
    cp -p dist-newstyle/build/x86_64-linux/ghc-$GHC_VERSION/cardano-cli-$CARDANO_TAG/x/cardano-cli/build/cardano-cli/cardano-cli ~/.local/bin
    cardano-cli --version

Get the config files
--------------------

.. code:: bash

    cd /data/Pool
    mkdir node
    cd node

    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/testnet-config.json
    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/testnet-shelley-genesis.json
    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/testnet-byron-genesis.json
    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/testnet-topology.json
    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-config.json
    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-byron-genesis.json
    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-shelley-genesis.json
    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-topology.json


