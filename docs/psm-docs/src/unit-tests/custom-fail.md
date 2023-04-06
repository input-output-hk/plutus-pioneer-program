## How to fail with custom conditions

We can also fail on custom conditions with function `logError`:

```haskell
unless checkCondition $ logError "Some invariant violated"
```


