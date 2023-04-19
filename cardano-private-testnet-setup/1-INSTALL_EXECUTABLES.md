# Install Cardano Executables

This guide covers installing `cardano-node`, `cardano-cli` and optionally `secp256k1` & `cardano-db-sync` into `$HOME/.local/bin`.

#### Assumptions
- This guide assumes you are running a Debian/Ubuntu linux OS.
  If you are using a different flavor of Linux, you will need to use the correct package manager for your platform
  
## Download/install the latest release tags of Cardano node and Cardano CLI executables

- Go to the [README page](https://github.com/input-output-hk/cardano-node#linux-executable) of the `cardano-node` project
  and you will see links to follow, where you can download the latest release binaries.
- Copy the binaries to local user path
  ```shell
  # extract cardano-cli and cardano-node from the archive
  # copy them to local path location
  cp cardano-cli $HOME/.local/bin/
  cp cardano-node $HOME/.local/bin/
  ```

If you don't plan on using `cardano-db-sync`, you can continue to guide: [3. Run Network Scripts](./3-RUN_NETWORK_SCRIPTS.md).

Otherwise, continue following the directions below.

***

**Note**: The remainder of this guide covers how to build the `cardano-db-sync` executables
from Haskell sources. **You may skip the rest of this readme, if db-sync is not relevant to you.**

## Optional: Building cardano-db-sync from Haskell sources using cabal and GHC

### 1. Install package dependencies and Haskell tooling
- Install package dependencies of tools
  ```shell
  # update/upgrade your package indexes
  sudo apt-get update
  sudo apt-get upgrade  
  # reboot as necessary
  sudo apt-get install automake build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libncursesw5 libtool autoconf -y  
  ```

- Install Cabal and GHC using [GHCUp - Haskell language installer](https://www.haskell.org/ghcup/)
  ```shell
  # download and run get-ghcup script
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
  # During questions, I chose to (A)ppend the path to ghc to .bashrc
  # and did not choose to install Haskell Language Server (HLS) or stack
  # source the bash start script to apply updates to PATH
  cd $HOME
  source .bashrc
  
  # get the latest updates to GHCUp tool
  ghcup upgrade

  # install cabal with GHCUp 
  ghcup install cabal 3.6.2.0
  ghcup set cabal 3.6.2.0

  # install GHC with GHCUp
  ghcup install ghc 8.10.7
  ghcup set ghc 8.10.7
  
  # Update cabal and verify the correct versions were installed successfully.
  cabal update
  cabal --version
  ghc --version
  ```

### 2. Install Libsodium library as pre-requisite for building cardano-db-sync

```shell
  # Clone the secp256k1 source
  cd $HOME/src
  git clone https://github.com/input-output-hk/libsodium
  
  # change directory
  cd libsodium
  
  # checkout specific branch    
  git checkout 66f017f1
  
  # apply configuration scripts and make project
  ./autogen.sh
  ./configure
  make
  sudo make install
```

### 3. Install secp256k1 library as pre-requisite for building cardano-db-sync
These directions are based on the script code found here: [Cardano-db-sync script file to setup secp256k1](https://github.com/input-output-hk/cardano-db-sync/blob/master/scripts/secp256k1-setup.sh)

  ```shell
  # Clone the secp256k1 source
  cd $HOME/src
  git clone https://github.com/bitcoin-core/secp256k1.git secp256k1
  
  # change directory
  cd secp256k1
  
  # reset the branch to appropriate commit    
  git reset --hard ac83be33d0956faf6b7f61a60ab524ef7d6a473a
  
  # apply configuration scripts and make project
  ./autogen.sh
  ./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental
  make
  make check

  # install library
  sudo make install
  ```
### 4. Install latest release tags of Cardano db-sync executables  

**Note**: The directions below are to build `cardano-db-sync` from Haskell sources using cabal and GHC.  If you want to explore other options to build
or deploy, e.g. using `nix-build` or `docker`,
please see the [IOHK cardano-db-sync README](https://github.com/input-output-hk/cardano-db-sync#readme) for more info.

- Clone the IOHK cardano-db-sync repo
  ```shell
  cd $HOME/src
  git clone https://github.com/input-output-hk/cardano-db-sync
  cd cardano-db-sync  
  # fetch the list of tags and check out the latest release tag name  
  git fetch --tags --all
  
  # checkout the latest release (currently 13.0.4 as of 8/22/22) of db-sync
  git checkout $(curl -s https://api.github.com/repos/input-output-hk/cardano-db-sync/releases/latest | jq -r .tag_name)
  ```

- Fetch postgres `libpq-dev` package, update dependencies and build the cardano-db-sync project.  This can take 20 minutes+
  **Note**: Building `cardano-db-sync` project from source, depends on finding the postgres `libpq-dev` package on the host OS.

  ```shell
  sudo apt-get install libpq-dev
  cabal update

  # build cardano-db-sync executables
  cabal build all
  ```

- Copy db-sync executables to local user default path location
  ```shell
  cp -p $(find dist-newstyle/build -type f -name "cardano-db-sync") $HOME/.local/bin/cardano-db-sync  
  ```

- Verify the versions of the db-sync executables
  ```shell
  cardano-db-sync --version
  # when this document was written, the current version for each is 13.0.4 on linux-x86_64
  ```
---
Continue to next guide: [2. Install PostgreSQL instructions](./2-INSTALL_POSTGRESQL.md)
