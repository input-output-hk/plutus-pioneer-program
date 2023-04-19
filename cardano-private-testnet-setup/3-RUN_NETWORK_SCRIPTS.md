# Run script to start private testnet

**Now, we get to the fun part!**  We are ready to create & start a private Cardano testnet.
The testnet will run three block producer nodes. 

#### Assumptions
- This guide assumes you are running a `babbage` compatible version of `cardano-cli`, i.e. 1.35.x. 
- These directions apply to Ubuntu (Debian). If you are using a different linux variant, please adjust as needed
- Before using this guide, you should have completed the [Install executables guide](./1-INSTALL_EXECUTABLES.md). 
  
## 1. Clone this project

  ```shell
  # navigate to working source directory
  cd $HOME/src
  git clone https://github.com/woofpool/cardano-private-testnet-setup
  ```

## 2. Run automate script to bootstrap the network

In this step, we run the `automate.sh` script.  This script will set up the necessary configuration files
for three block-producing nodes. The configuration data is compatible with the `babbage` era.  
After creating the configuration data, the script starts up the nodes.

Once you have completed this step, the `private-testnet` will be running in the `babbage` era.

#### Directions
- In **terminal #1**, run the automate script.
  It takes less than a minute to fire up the network.
  ```shell
  # navigate to project root folder
  cd $HOME/src/cardano-private-testnet-setup
  ./scripts/automate.sh
            
  # when the script completes successfully, it will show the current era and major protocol version
  # the last command that runs in the script is `wait`
  # If you kill the script, that will also kill the running nodes.
  ```
  - In **terminal #2**, you can tail the log to monitor the activity occurring in the nodes
  ```shell
  # navigate to project root folder
  cd $HOME/src/cardano-private-testnet-setup
  tail -f logs/mainnet.log
  ```

If you are planning to use `cardano-db-sync`, continue to guide: [4. Attach db-sync](4-ATTACH_DB_SYNC.md)

Otherwise, continue to guide: [5. Run transaction](5-RUN_TRANSACTION.md)