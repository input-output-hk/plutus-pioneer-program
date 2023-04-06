## How to query Blockchain

Let's reference various functions to query state of the blockchain

### Overloading by address

Often in signatures we will see the type class `HasAdress` for example:

```haskell
utxoAt :: HasAddress user => user -> Run [(TxOutRef, TxOut)]
```
Note the usage of `HasAddress` class. It means that the function is overloaded
over many types including `PubKeyHash`, `TypedValidator`, `AppendStaking a` etc.

### Query UTXOs

We can query UTXOs with functions:

```haskell
utxoAt :: HasAddress user => user -> Run [(TxOutRef, TxOut)]
```

we can pattern match if we expect certain amount of UTXOs:

```haskell
[(gameRef, gameUtxo)] <- utxoAt gameScript
```

It returns the list of UTXOs that belong to an address.
but note that it's going to fail with exception if there are no such UTXOs.
And the function `mustFail` can not recover from that.

We can use safer alternative that uses continuation:

```haskell
withUtxo ::
  HasAddress user
  => ((TxOutRef, TxOut) -> Bool)
  -> user
  -> ((TxOutRef, TxOut) -> Run ())
  -> Run ()
withUtxo isUtxo userAddr cont
```

It filter all UTXOs with predicate and invokes continuation if the UTXO is found.
If no UTXO found it logs an error with `logError`.

Also we have a function that always returns the first one:


```haskell
withFirstUtxo :: HasAddress user
  => user
  -> ((TxOutRef, TxOut) -> Run ())
  -> Run ()
```
Here the predicate `isUtxo` equals to `const True`.

#### Query reference scripts

In previous section we have learned how to query UTXOs. Note that UTXOs
that store reference scripts are not returned by those functions. For ease of use
we have different functions to query only UTXOs with reference scripts.
They have the same signatures only name is a bit different:

Return list of UTXOs:

```haskell
refScriptAt :: HasAddress user => user -> Run [(TxOutRef, TxOut)]
```

With continuation:

```haskell
withRefScript :: HasAddress user
  => ((TxOutRef, TxOut) -> Bool)
  -> user
  -> ((TxOutRef, TxOut) -> Run ())
  -> Run ()

withFirstRefScript :: HasAddress user
  => user
  -> ((TxOutRef, TxOut) -> Run ())
  -> Run ()
```

### Query Datums

To query datums we have function `datumAt`. Note that it queries both hashed and inlined
datums with the same interface:

```haskell
datumAt :: FromData a => TxOutRef -> Run (Maybe a)
```

also we have continuation style function:

```haskell
withDatum :: FromData a => TxOutRef -> (a -> Run ()) -> Run ()
```

It logs an error if no datum is found.

### Query Value to spend

we can query the UTXOs that user can spend to produce value:

```haskell
spend' :: PubKeyHash -> Value -> Run (Maybe UserSpend)
```

The `spend'` with tick is the safest approach to do it.
It returns `Nothing` if user does not have amount of value that user plans to spend
and `Just UserSpend` if everything is fine.
The `UserSpend` is a special structure that alongsied with spending UTXO also
contains the exchange UTXOs that user will pay back to ensure that all UTXOs are fully spent.

Note that this function does not performs the spend. It only calculates
the set of UTXOs that later can be used with the function `userSpend`.
To make the spend happen we need to submit signed transaction by the user.

also if we are sure that user has the required value we can use unsafe alternative:

```haskell
spend :: PubKeyHash -> Value -> Run UserSpend
```

Be aware of the errors like that:

```haskell
utxo1 <- spend user1 value1
utxo2 <- spend user1 value2
submitTx user1 $ toTx1 utxo1
submitTx user1 $ toTx2 utxo2
```

A careful reader may spot a problem.
It will cause troubles because `utxo1` and `utxo2` are calculated from the
same set of UTXOs that belong to the `user1` but after we submit first TX
the UTXO set is updated and `utxo2` is no longer valid. It might be valid if we are lucky
and user spend some UTXOs that are not present in `utxo2` but the right way to do it is:

```haskell
-- query UTXO to spend and update blockchain
utxo1 <- spend user1 value1
submitTx user1 $ toTx1 utxo1

-- query again on updated set of user1's UTXOs
utxo2 <- spend user1 value2
submitTx user1 $ toTx2 utxo2
```

So be aware of the query-nature of the `spend`. It does not updates the blockchain.
as the only way to update it is to submit a valid transaction or move it forward in time with
`wait`-family of functions.

also we have continuation-style function to query spends:

```haskell
withSpend :: PubKeyHash -> Value -> (UserSpend -> Run ()) -> Run ()
```

It queries UTXOs to spend and applies a continuation to it.
f there are no UTXOs to match the value it gracefully fails with `logError`.
It's the best function to use in tests as it can not fail with runtime exception.

### Query Boxes (Typed TxOuts)

We can query boxes (typed `TxOut`'s) with functions:

```haskell
boxAt :: (IsValidator script) => script -> Run [TxBox script]
```

It returns list of all boxes attached to the script.
If we are sure that there is only one we can use:

```haskell
nftAt :: IsValidator script => script -> Run (TxBox script)
```
It returns the single box but fails with runtime exception if no boxes found.
Ther safe approach to use in tests that are compatible with `mustFail` is
to use continuation based  functions:

```haskell
withBox :: IsValidator script
  => (TxBox script -> Bool)
  -> script
  -> (TxBox script -> Run ())
  -> Run ()
```

it behaves like `withUtxo`. It queries the list of boxes and searches for the
one we need with gven predicate `isBox`. If box is found the continuation is invoked
on it if there is no such box it logs error with `logError`.

Also we have funtion to query only first box:

```haskell
withNft :: IsValidator script
  => script
  -> (TxBox script -> Run ())
  -> Run ()
```


