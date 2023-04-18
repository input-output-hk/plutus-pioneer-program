# Install Cardano Executables

This guide covers installing `cardano-node`, `cardano-cli` and optionally `cardano-db-sync` into `$HOME/.local/bin`.

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
Continue to guide: [3. Run Network Scripts](./3-RUN_NETWORK_SCRIPTS.md)


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

### 2. Install latest release tags of Cardano db-sync executables including some patches necessary to make things work in private testnet  

**Note**: The directions below are to build `cardano-db-sync` from Haskell sources using cabal and GHC.  If you want to explore other options to build
or deploy, e.g. using `nix-build` or `docker`,
please see the [IOHK cardano-db-sync README](https://github.com/input-output-hk/cardano-db-sync#readme) for more info.

**Note2**: The directions below include applying some cherry-pick commits, which are necessary to allow `cardano-db-sync`
to work with a private testnet. If you want to understand the issue, please visit: [issue](https://github.com/input-output-hk/cardano-db-sync/issues/1046). 

- Clone the IOHK cardano-db-sync repo
  ```shell
  cd $HOME/src
  git clone https://github.com/input-output-hk/cardano-db-sync
  cd cardano-db-sync  
  # fetch the list of tags and check out the latest release tag name  
  git fetch --tags --all
  
  # checkout the latest release (currently 12.0.2 as of 3/27/22) of db-sync
  git checkout $(curl -s https://api.github.com/repos/input-output-hk/cardano-db-sync/releases/latest | jq -r .tag_name)

  # cherry pick the following 2 commits (using -n to avoid auto committing into local git repo)

  # cherry-pick #1 - change to Genesis.hs
  git cherry-pick -n bff6e182cf41f6f7a9ff3d08bb1fc9984e2d0f66

  # cherry-pick #2 - change to Block.hs
  git cherry-pick -n 132f569bcd297ce73bca407a52acee513aa75389
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
  cp -p $(find dist-newstyle/build -type f -name "cardano-db-sync-extended") $HOME/.local/bin/cardano-db-sync-extended  
  ```

- Verify the versions of the db-sync executables
  ```shell
  cardano-db-sync --version
  cardano-db-sync-extended --version
  # when this document was written, the current version for each is 12.0.2 on linux-x86_64
  ```
---
Continue to next guide: [2. Install PostgreSQL instructions](./2-INSTALL_POSTGRESQL.md)