# Attach db-sync process to the private network

---
**Note**: If you have not installed `cardano-db-sync`, then you should skip this guide and continue to next guide: [5. Run transaction](5-RUN_TRANSACTION.md)
This guide covers attaching the `cardano-db-sync` process to the private testnet.  Doing so causes the blockchain data to
be written to a highly normalized database schema in PostgresSQL database.  Then, it's possible to run SQL queries against the data
for easy retrieval of information.

#### Assumptions

- This guide assumes you are running a recent version of linux.
  Specifically, these directions apply to Ubuntu (Debian). If you are using a different linux variant, please adjust as needed
- Before using this guide, you should have completed the [Install executables guide](./1-INSTALL_EXECUTABLES.md) and
  [Install PostgreSQL guide](2-INSTALL_POSTGRESQL.md).

## Run the script to start up the cardano-db-sync process

- Make sure the nodes are running
- If necessary, please modify the `SCHEMA_DIR` environment variable below based on the location of your cloned copy of cardano-db-sync project
- In **terminal 3**, start the db sync process.  This will install the database schema and sync blockchain data to the Postgres database.
  ```shell
  # navigate to project root folder
  cd $HOME/src/cardano-private-testnet-setup
  # set environment variable needed by `./scripts/db-sync-start.sh`
  export SCHEMA_DIR=$HOME/src/cardano-db-sync/schema  
  # run script file
  ./scripts/db-sync-start.sh  
  # output
  # verify the output does not show any errors
  # in a steady state, you should see logs of SQL insert statements into slot_leader and block tables   
  ```
---

Continue to next guide: [5. Run transaction](5-RUN_TRANSACTION.md)