# How to use custom coins

Often we need to use some custom coins to test the task.
To create new coin we usually need to create the minting policy script for it and
setup rules to mint the coins.

To make this task easy there are "fake" coins provided by the library.
To create fake coin we can use functions:

```haskell
testCoin :: FakeCoin
testCoin = FakeCoin "test-coin-A"

fakeValue :: FakeCoin -> Integer -> Value
fakeCoin  :: FakeCoin -> AssetClass
```

With those functions we can assign some fake coins to admin user on start up of the blockchain:

```haskell
testValue = fakeValue testCoin
mock = initMock config (adaValue 1000_000 <> testValue 1000)
```

In the blockchain code we can give those fake coins to users when we create them:

```haskell
u1 <- newUser (adaValue 100 <> testValue 10)
```

Note that when we send custom value from one user to another we also have to send some minimal ada value.
In Cardano every UTXO should have some ada in it. EmulatorTrace hides those details but
in this test framework it's exposed to the user.

So to send 5 test coins from one user to another add some ada to it:

```haskell
sendValue user1 (adaValue 1 <> testValue 5) user2
```



