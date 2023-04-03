# Log debug information

Sometimes it's useful to print message for the user signaling specific
stage of execution or which values certain datum has.

For that we can use function:

```haskell
**logInfo** :: String -> Run ()
```

It will add the message to the internal logs.

## Logging blockchain state

The ability to observe what's going on in the blockchain is a great way to understand things.
To log balances use `logMockState` action, which saves current state as a log entry. To show
log use `testNoErrorsTrace` helper (see below).



