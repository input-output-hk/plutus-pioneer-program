# How to check balances

There are useful functions to check not absolute balances but balance transitions.
We have type `BalanceDiff` and we can construct it with function:

```haskell
owns  :: 
     HasAddress addr 
  => addr -> Value -> BalanceDiff
```

It states that address gained so many coins. We have useful function to check the move of values
from one address to another

```haskell
gives :: 
     (HasAddress addrFrom, HasAddress addrTo) 
  => addrFrom -> Value -> addrTo -> BalanceDiff
```

The type `BalanceDiff` is a monoid. So we can stack several transitions with `mappend` and `mconcat`.

To check the balances we use function:

```haskell
checkBalance :: BalanceDiff -> Run a -> Run a
```

It queries balances before application of an action and after application of the action and
reports any errors. If balances does not match to expected difference.

For example, we can check that sendValue indeed transfers value from one user to another:

```haskell
checkBalance (gives u1 val u2) $ sendValue u1 val u2
```

If we want to check that user or script preserved the value we can set it up with `(owns user mempty)`.
Because TypedValidator has instanced of the HasAddress we can also check payments to/from scripts:

```haskell
checkBalance (gives pkh prize gameScript) $ do { ... }
```

Sometimes we need to check balances based on result of the action. For that we have generic 
function `checkBalanceBy`:

```haskell
checkBalanceBy :: (a -> BalanceDiff) -> Run a -> Run a
```

See more examples at tests for `Game` and `Counter` scripts.



