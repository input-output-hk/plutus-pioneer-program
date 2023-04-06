## How to skip messages from the logs

All TXs are logged, sometimes we do auxiliary TX that are irrelevant for testing,
and we want to skip the logging of them. For this case we can use function

```haskell
noLog :: Run a -> Run a
```

It executes an action and during execution it omits all logging of TXs and info level messages.
But errors are logged. If we want more fine-grain control we can use `noLogTx` and `noLogInfo`.


