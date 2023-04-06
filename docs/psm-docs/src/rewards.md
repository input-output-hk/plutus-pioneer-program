# Certificates and withdrawals of the rewards

In cardano staking pools are driving force behind confirmation of transactions.
For the work pool owners receive fees. The fees get distributed among 
staking credentials that delegate to pool. 

So we have staking pool operator (SPO) who is chosen for this round to
confirm TX. And for this work SPO gets the fees. Fees are collected 
to the addresses that are called staking credentials. Users can delegate staking credential
to the pool owner (SPO). 

So far we have ignored the fees, but in real scenario every transaction should contain fees.
To pay fee in the transaction we can use function. The fees are fairly distributed among
all staking credentials that belong to the pool:

```haskell
-- Pay fee for TX confirmation. The fees are payed in ADA (Lovelace)
payFee :: Ada -> Tx
```
The `Ada` is a newtpye wrapper over `Integer` that specifies amount of lovelaces.
It is defined in the module `Plutus.Model.Ledger.Ada`.


#### How reward distribution works

For each round of TX confirmation there is a pool called **leader** that confirms
TX. For that the pool receives fees and fairly distributes them among stake credentials
that delegate to that pool.

For this testing framework we go over a list of pools one by one and on each TX-confirmation
we choose the next pool. All staking credentials delegated to the pool receive equal amount of fees.
When blockchain initialised we have a single pool id and stake credential that 
belongs to the admin (user who owns all funds on genesis TX). 

So to test some Stake validator we need to register it in the blockchain
and delegate it to admin's pool. We can get the pool of the admin by calling `(head <$> getPools)`.
This gives the pool identifier of the only pool that is registered.
After that we create TX that registers taking credential and delegates it to admin's pool.
We can provide enough fees for that TX to check the stake validator properties. 

As an example we can use the test-case in the module `Suites.Plutus.Model.Script.Test.Staking`.
It implements a basic stake validator and checks that it works (see the directory `test` for this repo).


#### Certificates

To work with pools and staking credentials we use certificates (`DCert` in Plutus).
We have functions that trigger actions with pools and staking credentials:

First we need to register staking credential by pub key hash (rewards belong to certain key)
or by the script. For the script there is guarding logic that regulates spending of rewards
for staking credential. For the key we require that TX is signed by the given key to spend the reward.

```haskell
registerStakeKey    :: PubKeyHash -> Tx

registerStakeScript :: TypedStake redeemer -> Tx
```

Also we can deregistrate the staking credential:

```haskell
deregisterStakeKey    :: PubKeyHash -> Tx

deregisterStakeScript :: IsValidator (TypedStake redeemer) => 
  TypedStake redeemer -> redeemer -> Tx
```

A staking credential can get rewards only over a pool. To associate it with pool
we use the function delegate:

```haskell
delegateStakeKey    :: PubKeyHash -> PoolId -> Tx

delegateStakeScript :: IsValidator (TypedStake redeemer) => 
  TypedStake redeemer -> redeemer -> PoolId -> Tx
```

The type `PoolId` is just a `newtype` wrapper for `PubKeyHash`:

```haskell
newtype PoolId = PoolId { unPoolId :: PubKeyHash }
```

As blockchain starts we have only one pool and staking credential associated with it.
It is guarded by admin's pub key hash.

We can register or deregister (retire) pools with functions:

```haskell
registerPool :: PoolId -> Tx
retirePool   :: PoolId -> Tx
```
Alas above functions do not work (need to fix conversion to VrfKey) at the moment.
Use direct insertion/removal of pools with `insertPool` and `deletePool` to manage stake pools:

```haskell
insertPool :: PoolId -> Run ()
deletePool :: PoolId -> Run ()
```

For staking validators each of those functions is going to trigger validation with purpose
`Certifying DCert`. Where `DCert` can be one of the following:

```haskell
data DCert
  = DCertDelegRegKey StakingCredential     -- register stake
  | DCertDelegDeRegKey StakingCredential   -- deregister stake
  | DCertDelegDelegate                     -- delegate stake to pool
      StakingCredential
      -- ^ delegator
      PubKeyHash
      -- ^ delegatee
  | -- | A digest of the PoolParams
    DCertPoolRegister                     -- register pool
      PubKeyHash
      -- ^ poolId
      PubKeyHash
      -- ^ pool VFR
  | -- | The retiremant certificate and the Epoch N
    DCertPoolRetire PubKeyHash Integer   -- retire the pool 
                                         -- NB: Should be Word64 but we only have Integer on-chain
  | -- | A really terse Digest
    DCertGenesis                         -- not supported for testing yet
  | -- | Another really terse Digest
    DCertMir                             -- not supported for testing yet
```

#### Query staking credentials and pools

We can collect various stats during execution on how many coins we have for rewards
and where staking credential belongs to.

```haskell
-- get all pools
getPools :: Run [PoolId]

-- get staking credential by pool
stakesAt :: PoolId -> Run [StakingCredential]

-- get rewards for a StakingCredential, PubKeyHash and StakeValidator or TypedStake
rewardAt :: HasStakingCredential cred => cred -> Run Integer

-- query if pool is registered
hasPool :: PoolId -> Run Bool

-- query if staking credential is registered
hasStake :: HasStakingCredential cred => cred -> Run Bool
```

#### Utility functions to create staking credentials

We can create staking credential out of pub key hash or stake validator
with function:

```haskell
toStakingCredential :: HasStakingCredential a => a -> StakingCredential
```

#### Withdrawals of the rewards

When staking credential has some rewards we can withdraw it and send
to some address. To do it we need to spend all rewards, so we need to 
provide exact amount that is stored in the rewards of the staking credential
at the time of TX confirmation. 

We can query the current amount with function `rewardAt`:

```haskell
-- get rewards for a StakingCredential
rewardAt :: StakingCredential -> Run Integer
```

To get rewards we need to include in transaction this part:

```haskell
withdrawStakeKey    :: PubKeyHash -> Integer -> Tx

withdrawStakeScript :: (IsValidator script) => 
  TypedStake script -> RedeemerType script -> Integer -> Tx
```

As with certificates we can withdraw by pub key hash (needs to be signed by that key)
and also by validator. 

To test withdraws we need to have rewards. We can easily generate rewards by 
making transactions that contain fees granted with `payFee` function.


