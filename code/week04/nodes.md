# Notes Week4

## Setup
> checkout plutus commit from cabal.project file
2fbb7abb22138a434bb6c4f663a81e9b9dc51e98

## Side Effects with Haskell
Plutus applies *referencial transparency*, this is also the case for Functions with Side Effects as the only get executed from the main module
`foo = IO Int`
means after the side effect has executed it produces a string

## Execute Hello World example

in the *.cabal file the executable hello.hs is defined (Previously there were only library stanzas as we only wrote libraries). Therefore we can execute the haskell script from the command line with the following command
`cabal run hello`
There we can use and check the input output functionality like reading from the console and printing out to the console.
### execute the action in the repl
`cabal repl`
`putStrLn "Hello World"`

## Functors
[functors learn you a Haskell](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#the-functor-typeclass)
```haskell
class Functor f where  
    fmap :: (a -> b) -> f a -> f b  
```
`fmap (map toUpper) getLine :: IO [Char]`
applies 
`fmap  (map toUpper) getLine`

## Sequence Operator
`>>` Chans together IO operation ignores the result of the first execution

## bind Operator
`>>=`
Does not ignore the result of the first execution
`getLine >>= putStrLn`

## Maybe Type
Similar to Optional in java except in java a Regular type in java could still be null, in Haskell this is not possible
`Text.Read (readMaybe)` can return a `Just` or a `Nothing`

## Either
Returns Either `Left` or `Right`
```haskell
readEither :: Read a => String -> Either String a
readEither s = case readMaybe s of
    Nothing -> Left $ "can't parse: " ++ s
    Just a  -> Right a
```

## do notation for Monads

```haskell
threeInts :: Monad m => m Int -> m Int -> m Int -> m int
threeInts mx my mz =
    mx >>= \k ->
    my >>= \l ->
    mz >>= \m ->
    let s = k + l + m in return s
```

can be written as
```haskell
threeInts' :: Monad m => m Int -> m Int -> m Int -> m int
threeInts' mx my mz = do
    k <- mx
    l <- my
    m <- mz
    let s = k + l + m
    return s
```

# Must know
> Contract is a Monad -> therefore we can use the do notation
Superfeatures a Contract Monad has
* Contract Monad is the Offchain Part of Plutus
* In the Writer Monad there is the tell function is available (Log messages)

# Trace
in Cabal repl
`:l src/Week04/Trace.hs`
`import Plutus.Contract.Trace`
Then we can use `defaultDist` for the default Token distribution where each Wallet gets 100 ADA
We can specify the Wallets for the distribution if needed
`defaultDist [Wallet 1, Wallet 2]`

