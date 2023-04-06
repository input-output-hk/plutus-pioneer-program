# Testing with tasty

In real unit test suite we are likely to use `tasty` to create tests. For that we have
helper function that checks that no errors has happened during execution:

```haskell
testNoErrors :: Value -> MockConfig -> String -> Run a -> TestTree
testNoErrors totalAdminFunds config testName action
```

with this function we can check the script in simpler way:

```haskell
good :: String -> Run a -> TestTree
good = testNoErrors initFunds defaultAlonzo

test :: TestTree
test = good "User can exchange values" userExchange

userExchange :: Run ()
userExchange = do
  users <- setupUsers                -- create 3 users and assign each 1000 lovelaces
  let [u1, u2, u3] = users           -- give names for users
  sendValue u1 (adaValue 100) u2     -- send 100 lovelaces from user 1 to user 2
  sendValue u2 (adaValue 100) u3     -- send 100 lovelaces from user 2 to user 3
```

Also we have more convenient way to check value transfers with function `checkBalance`:

```haskell
checkBalance :: BalanceDiff -> Run a -> Run a
```

it checks that certain value transfers has happened during execution of an `Run`-action.
If difference is not as expected it logs an error.


To create `BalanceDiff` we use functions:

```haskell
-- | Balance difference constructor
owns  :: HasAddress user => user -> Value -> BalanceDiff

-- | User A gives value to user B.
gives :: (HasAddress userA, HasAddress userB) => userA -> Value -> userB -> BalanceDiff
gives from val to = ...
```

Note that we check balance difference in relative values not in absolute ones. 
So instead of querying the values and checking the absolute values as in previous example:

```haskell
  ...
  vals <- mapM valueAt users         -- read user values
  pure $ and                         -- check test predicate
    [ isOk
     , vals == fmap adaValue [900, 1000, 1100]
    ]
```

We can check relative transitions:

```haskell
  let [u1, u2, u3] = users           -- give names for users
  checkBalance (gives u1 (adaValue 100) u2 <> gives u2 (adaValue 100) u3) $ do
      sendValue u1 (adaValue 100) u2     -- send 100 lovelaces from user 1 to user 2
      sendValue u2 (adaValue 100) u3     -- send 100 lovelaces from user 2 to user 3
```

This check seems to be obvious but in real test cases we can check 
transitions of values from users to scripts and between scripts.
Also we can check that user or script has not changed the value by `owns u1 mempty`.
In this way we enforce that certain actions preserve the value for user `u1`.


