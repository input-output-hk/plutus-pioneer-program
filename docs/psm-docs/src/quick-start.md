# Quick start

We can create simple mock blockchain data structure and update within context of State monad.
Update happens as a pure function and along TX-confirmation we have useful stats to estimate usage of resources.
Internaly it uses `evaluateTransactionExecutionUnits` and `evaluateTransactionBalance` 
functions from `cardano-ledger` to check that TX is valid.

To create mock blockchain we first need to specify blockchain config (`MockConfig`).
Config is specified by protocol parameters (`PParams`) and era history (`EraHistory CardanoMode`).
They are cardano config data types. To avoid gory details it's safe to use predefined config
and default parameters for Alonzo era:

```haskell
defaultAlonzo :: MockConfig
```

also we can use parameters for Babbage era:

```haskell
defaultBabbage :: MockConfig
```

The parameters era determine which era is going to be used for TX-representation and validation checks.

Once we have config available we can create initial state for blockchain with function:

```haskell
initMock :: MockConfig -> Value -> Mock
initMock config adminValue = ...
```

It creates blockchain that has only one UTXO that belongs to the admin user. The value is how many coins
we are going to give to the admin. Admin can distribute values to test users from admin's reserves.

The rest of the code happens within `Run` monad which is a thin wrapper on State over `Mock`-blockchain under the hood.
We have scenarios of script usages as actions in the `Run` monad. When we are done we can get the result:


```haskell
runMock :: Run a -> Mock -> (a, Mock)
```

It just runs the state updates.

Let's create test users:

```haskell
-- | alocate 3 users with 1000 lovelaces each
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 3 $ newUser $ adaValue 1000
```

`adaValue` is standard function that creates singleton ada `Value` out of amount of **lovelaces**.
It's defined in the module `Plutus.Model.Ada`.
We can create new user and send values from admin to the user with the function:

```haskell
newUser :: Value -> Run PubKeyHash
```

Users are identified by their pub key hashes. Note that admin should have the value that we want to share
with new user. Otherwise we will get run-time exception.

Users can send values to each other with function:

```haskell
sendValue :: PubKeyHash -> Value -> PubKeyHash -> Run ()
```

Let's share some values:

```haskell
simpleSpend :: Run Bool
simpleSpend = do
  users <- setupUsers                -- create 3 users and assign each 1000 lovelaces
  let [u1, u2, u3] = users           -- give names for users
  sendValue u1 (adaValue 100) u2     -- send 100 lovelaces from user 1 to user 2
  sendValue u2 (adaValue 100) u3     -- send 100 lovelaces from user 2 to user 3
  isOk <- noErrors                   -- check that all TXs were accepted without errors
  vals <- mapM valueAt users         -- read user values
  pure $ and                         -- check test predicate
    [ isOk
     , vals == fmap adaValue [900, 1000, 1100]
    ]
```

This example shows how we can create simple tests with library.
We create three users and exchange the values between the users.
In the test we check that there are no errors (all TXs were accepted to blockchain) and
that users have expected values.

To check for TX errors we use:

```haskell
noErrors :: Run Bool
```

Blockchain logs all failed transactions to the list `mockFails`. We check that this list is empty.

To read total value for the user we use:

```haskell
valueAt :: HasAddress addr => addr -> Run Value
```

Complete working example of user exchange can be found at test suites (see `Suites.Plutus.Model.User`)


