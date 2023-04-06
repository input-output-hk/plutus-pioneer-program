# Log custom errors

We can log custom errors with

```haskell
logError :: String -> Run ()
```

Errors are saved to log of errors. This way we can report our own errors based on conditions
using functions `when` and `unless`.
If values are wrong or certain NFT was not created etc.

Also, we can log information with:

```haskell
logInfo :: String -> Run ()
```

It saves information to the log. The log of errors is unaffected.
We can read log messages with function:

```haskell
getLog :: Mock -> Log MockEvent
```

Where MockEvent is one of the events:

```haskell
data MockEvent
  = MockTx TxStat           -- ^ Sucessful TXs
  | MockInfo String         -- ^ Info messages
  | MockFail FailReason     -- ^ Errors
```



