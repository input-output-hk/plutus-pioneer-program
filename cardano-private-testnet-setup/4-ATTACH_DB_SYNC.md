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

- Make sure the private network node processes are running
- In a new termimal, create the db-sync database using the script file utility in `cardano-db-sync`
  ```shell
  # set session variable to the root directory of your cardano-db-sync project sources 
  CARDANO_DB_SYNC_HOME=$HOME/src/cardano-db-sync

  # navigate to top level folder of this project
  cd $HOME/src/cardano-private-testnet-setup
  
  # set the permissions of the postgres password file
  chmod 0600 postgres-conn/pgpass-privatenet
  
  # create a database named privatenet
  PGPASSFILE=postgres-conn/pgpass-privatenet $CARDANO_DB_SYNC_HOME/scripts/postgresql-setup.sh --createdb              
  ```
- Verify the empty database has been created
  ```shell
  # open psql command prompt as sudo user
  sudo -u postgres psql
  
  # list databases
  \l
  
  # verify privatenet database is listed
  # quit psql 
  \q    
  ```
- Start the db sync process.  This will install the database schema in the privatenet database and sync blockchain data to the Postgres database.
  ```shell
  # set the SCHEMA_DIR session variable to the schema folder from the cardano-db-sync project sources
  export SCHEMA_DIR=$CARDANO_DB_SYNC_HOME/schema  
  
  # run script file
  cd $HOME/src/cardano-private-testnet-setup
  ./scripts/db-sync-start.sh  
  # output
  # verify the output does not show any errors
  # in a steady state, you should see logs of SQL insert statements into slot_leader and block tables   
  ```
- Troubleshooting
  - You may want to drop the `privatenet` database to get a clean start
    ```shell
    cd $HOME/src/cardano-private-testnet-setup
    PGPASSFILE=postgres-conn/pgpass-privatenet $CARDANO_DB_SYNC_HOME/scripts/postgresql-setup.sh --dropdb
    ```
---

Continue to next guide: [5. Run transaction](5-RUN_TRANSACTION.md)