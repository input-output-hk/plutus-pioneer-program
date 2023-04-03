# How to use in unit tests

For convenience there is a function `testNoErrors`:

```haskell
testNoErrors :: Value -> MockConfig -> String -> Run a -> TestTree
testNoErrors totalMockFunds mockConfig testMessage script = ...
```

It checks that given script runs without errors. Note that if we want to use custom
checks on run-time values we can query checks inside the script and log errors with `logError`.
By default, it also checks for resource usage constraints.

A more verbatim alternative is `testNoErrorsTrace` which is the same, but prints out 
the blockchain log on both failures and successes. The output might be rather excessive,
so use it judiciously. Probably it's worth using one of these functions depending on the
runtime "dump log" parameter value (if you use `tasty` you might want to use an `Ingredient`
to implement this).

If we want to check only logic but not resource usage we can use:

```haskell
skipLimits :: MockConfig -> MockConfig
```

Also, there is function `warnLimits` that logs errors of resources usage
but does not fail TX for submission. So if logic is correct script will run but
errors of resources will be logged to error log.


