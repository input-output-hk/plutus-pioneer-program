# Install PostgreSQL and create user
**Note**: If you have not installed `cardano-db-sync`, then you should skip this guide and continue to next guide: [3. Run scripts](3-RUN_NETWORK_SCRIPTS.md)

This document explains how to install PostgreSQL and create a Postgres user.

#### Assumptions

- This guide assumes you are running a recent version of linux. 

  Specifically, these directions apply to Ubuntu (Debian). If you are using a different linux variant, please adjust as needed
   

## 1. Install PostgreSQL
 
- Update/upgrade your package indexes

  ```shell

  sudo apt-get update

  sudo apt-get upgrade

  # reboot as necessary  

  ```

- Install postgreSQL packages

  ```shell

  sudo apt-get install postgresql postgresql-contrib

  ```

- Verify postgres is installed by starting a postgres sql session in the terminal

  ```shell

  sudo -u postgres psql
  
  # you should see prompt

  # postgres=#
  
  # to exit the session 

  \q

  ```

## 2. Create new Postgres role for your linux user account

- Upon installation, Postgres is set up to use `ident authentication`, 

  meaning that it associates Postgres roles with a matching Unix/Linux system account. 

  If a role exists within Postgres, a Unix/Linux username with the same name is able to sign in as that role.

  - Additional background documentation: [Postgresql Ident Authentication](https://www.postgresql.org/docs/current/auth-ident.html)  
    
- Create a user for your local linux user account and give it superuser role.   Our user must be a superuser in order to create/drop databases, etc.

  ```shell

  sudo -u postgres createuser --interactive
  
  # enter your linux user account

  Enter name of role to add: <your_linux_user_account_name>

  Shall the new role be a superuser? (y/n) y

  ```

- Verify your local account got created

  ```shell

  sudo -u postgres psql
  
  # you should see prompt

  # postgres=#
  
  # to display users 

  \du
  
  # you should see a role name of your linux account with Superuser role attribute
  
  # to exit the session 

  \q

  ```

---

Continue to next guide: [3. Run scripts](3-RUN_NETWORK_SCRIPTS.md)