# Run script to start private testnet

**Now, we get to the fun part!**  We are ready to create & start a private Cardano testnet.
The testnet will run three block producer nodes. 

#### Assumptions
- This guide assumes you are running a recent version of linux.
  Specifically, these directions apply to Ubuntu (Debian). If you are using a different linux variant, please adjust as needed
- Before using this guide, you should have completed the [Install executables guide](./1-INSTALL_EXECUTABLES.md). 
  
## 1. Clone this project

  ```shell
  # navigate to working source directory
  cd $HOME/src
  git clone https://github.com/woofpool/cardano-private-testnet-setup
  ```

## 2. Run automate script to bootstrap the network

In this step, we run the `automate.sh` script.  This script will set up the necessary configuration files
for three block-producing nodes. The configuration data is compatible with the `alonzo` era.  
After creating the configuration data, the script starts up the nodes and fires an `update-1.sh` script to run a transaction that sends
genesis funds to the user1 wallet address.

Once you have completed this step, the `private-testnet` will be running in the `alonzo` era and the user1 wallet address
has some `UTxOs` to use for doing transactions.

#### Directions
- Adjust the [config.cfg file](./scripts/config.cfg) as desired. By default, the ROOT directory is set to `private-network`
- In **terminal #1**, run the automate script.
  It takes less than a minute to fire up the network.
  ```shell
  # navigate to project root folder
  cd $HOME/src/cardano-private-testnet-setup
  ./scripts/automate.sh
      
  # if you receive an error about "running nodes found", you will need to kill cardano node processes
  # and run the automate.sh script again
      
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

## 3. Verify the network state and wallet UTxO balances of user1

- After completing the full set of updates, verify the era and utxo balances of user1 address
  ```shell
  # if necessary, set variables
  ROOT=private-testnet
  export CARDANO_NODE_SOCKET_PATH=$ROOT/node-bft1/node.sock
  
  cardano-cli query tip --testnet-magic 42 | jq '.era'
  #output
  "Alonzo"
  
  cardano-cli query utxo --address $(cat private-testnet/addresses/user1.addr) --testnet-magic 42
  #output
                            TxHash                                 TxIx        Amount
  --------------------------------------------------------------------------------------
  b0f91ee59eb208284467b1dec0adfa8c57eb1cf7587fb7eb0599e2b8c8e885c9     0        500000000 lovelace + TxOutDatumHashNone
  b0f91ee59eb208284467b1dec0adfa8c57eb1cf7587fb7eb0599e2b8c8e885c9     1        500000000 lovelace + TxOutDatumHashNone
  ``` 
- **Troubleshooting**: If you run `cardano-cli query tip` and the blocks are not advancing or the syncProgress percent is decreasing,
  it may mean the processes running the nodes are running into garbage collection/memory issues. The author is still researching
  the cause of this issue.  In any event, the best remedy is killing the run node processes, deleting the `private-testnet` folder
  and starting over.  This garbage collection issue normally happens early in the update process.
    - If you use Ctrl+c in the window running the `run/all.sh` script, it should kill the processes that started in the background.
      Another approach is to kill the processes directly by doing:
      ```shell
      for PID in `ps -ef | grep 'cardano-node' | grep -v grep |  awk '{print $2}'`;do kill -TERM $PID; done      
      ```
    
---

If you are planning to use `cardano-db-sync`, continue to guide: [4. Attach db-sync](4-ATTACH_DB_SYNC.md)

Otherwise, continue to guide: [5. Run transaction](5-RUN_TRANSACTION.md)