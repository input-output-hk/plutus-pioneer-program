## How to check TX resource usage

Sometimes we write too much code in the validators, and it starts to exceed execution limits.
TXs like this can not be executed on chain. To watch out for that we have special function:

```haskell
testLimits ::
   Value
   -> MockConfig
   -> String
   -> (Log TxStat -> Log TxStat)
   -> Run a
   -> TestTree
testLimits totalMockFunds mockConfig testMessage logFilter script
```

Let's break apart what it does. It runs blockchain with limit check config set to @WarnLimits@.
This way we proceed to execute TX on blockchain even if TX exceeds the limits, but we save the
error every time it happens. When script was run if resource usage errors are encountered
they are logged to the user in easy to read way.

To see the logs even on successful run we can add fake error:

```haskell
(script >> logError "Show stats")
```

The results are shown in the percentage to the mainnet limit. We need to care
that all of them are below 100%. Also note that we'd better have some headroom 
and keep it not close to 100% because number of inputs in TX is unpredictable.
we can aggregate input values for the scripts from many UTXOs. So we'd better have 
the free room available for extra UTXOs.

Filter of the log can be useful to filter out some non-related events. for example setup of the blockchain
users. We typically cn use it like this:

```haskell
good "Reward scripts" (filterSlot (> 4)) (Rewards.simpleRewardTestBy 1)
  where
    good = testLimits initFunds cfg
```

It's good to implement complete set of unit tests first and then add limits tests.
So that every transformation to optimise on resources is checked by ordinary unit tests.
On unit tests we can skip limit checks with `skipLimits :: MockConfig -> MockConfig`.


