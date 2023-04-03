# Testing scripts

In this chapter we will learn how to work with scripts.
As an example we are going to test the hash game script.
Let's initialise the game. For that we spend a prize to the UTXO guarded
by game validator:

```haskell
initGame :: PubKeyHash -> Value -> BuiltinByteString -> Run ()
initGame pkh prize answer = do   -- args: user, prize value , answer for puzzletype
  sp <- spend pkh prize          -- read users UTXO that we should spend
  submitTx pkh $                 -- create TX, sign it and post to 
    initGameTx sp prize answer   -- ledger with user's secret key

-- pure function to create TX
initGameTx :: UserSpend -> Value -> BuiltinByteString -> Tx
```

Let's discuss the function bit by bit. To create TX we need to first determine the inputs.
Input is set of UTXO's that we'd like to spend. For that we use function

```haskell
spend :: PubKeyHash -> Value -> Run UserSpend
```

For a given user and value it returns a value of `UserSpend` which holds
a set of input UTXOs that cover the value that we want to spend, and also it has change to spend back to user.
In the UTXO model we can not split the input. If it's bigger than we need we have to destroy it
and create one UTXO that is given to someone else and another one that is spent back to user.
We can think about the latter UTXO as a change.

Note that it is going to produce run-time exception if there are not enough funds to spend.
To avoid run-time errors there is safer variant called `spend'`.
Also, we can use safer alternative `withSpend`. It logs an error
if user has no funds to spend and continues execution which can
be helpful in some test cases:

```haskell
withSpend :: PubKeyHash -> Value -> (UserSpend -> Run ()) -> Run ()
```

When we know what inputs to spend we need to make a TX. We do it with function `initGameTx`. We will discuss it soon.
After that we should sign TX with the key of the sender and send it to
ledger. We use function `submitTx` for that.

Let's discuss how to create TX:

```haskell
initGameTx :: UserSpend -> Value -> BuiltinByteString -> Tx
initGameTx usp val answer =
  mconcat
    [ userSpend usp
    , payToScript gameScript (HashDatum $ GuessHash $ Plutus.sha2_256 answer) val
    ]
```

We create transaction by accumulation of monoidal parts. As Plutus Tx is monoid it's convenient
to assemble it from tiny parts. For our task there are just two parts:

* for inputs and outputs of user spend
* pay prize to the script and form right datum for it.

We use function to make right part for spending the user inputs and sending change back to user:

```haskell
userSpend :: UserSpend -> Tx
```

To pay to script we use function:

```haskell
data DatumMode a 
  = HashDatum a      -- ^ store datum hash in TxOut
  | InlineDatum a    -- ^ store inlined datum value in TxOut

payToScript :: 
     (HasAddress script, HashDatum script)
  => script 
  -> DatumMode (DatumType script) 
  -> Value 
  -> Tx
```

So it uses address of the validator, datum for it (of proper type) and value to protect with the contract.
As simple as that. Our type `Game` is `TypedValidator GameDatum GameRedeemer` and
for typed valdiator first type argument corresponds to `DatumType`.
As input we can use `TypedValidator`, `TypedValidatorHash` and `AppendStaking`-wrappers.

Note that in example we wrap it in `HashDatum`. Starting from Babbage era 
we can store not only datum hashes in `TxOut` but also we can inline datum values
stright into `TxOut`. To distinguish between two cases we use `DatumMode` wrapper.
It's available from Babbage era. In Alonzo era we can use only `HashDatum` mode.

Let's create another Tx to post solution to the puzzle. It seems to be more involved but don't be scary.
We will take it bit by bit:

```haskell
guess :: PubKeyHash -> BuiltinByteString -> Run Bool
guess pkh answer = do
  utxos <- utxoAt gameScript             -- get game script UTXO
  let [(gameRef, gameOut)] = utxos       --   we know there is only one game UTXO
  mDat <- datumAt @GameDatum gameRef     -- read game's datum
  case mDat of                           -- if there is a datum
    Just dat -> do                       -- let's create TX, sign it and submit
      submitTx pkh $ guessTx pkh gameRef (txOutValue gameOut) dat answer
    Nothing -> logError "No datum"
```

This function is a bit bigger because now we want to spend not only our funds but also the fund of the script.
For that we look up the script UTXO (`utxoAt`) and look up its datum (`datumAt`) and when it all succeeds
we can form the right TX, sign it with our key and post it to blockchain.

Functions that query blockchain are often defined on addresses or `TxOutRef`'s:

```haskell
utxoAt  :: HasAddress addr => addr -> Run [(TxOutRef, TxOut)]
datumAt :: TxOutRef -> Run (Maybe a)
```
Note that `datumAt` reads both hashed and inlined datums with the same interface.

With `logError` we can report custom errors. It will save the error to the log
and the whole test will fail with `testNoErrors` function or `noErrors` function.

Our `gameScript` has instance of `HasAddress`. It is an address of underlying script.
We should query the datum separately because `TxOut` contains only hash of it.
Let's look at the pure function that creates TX. Again we assemble TX from monoidal parts:

```haskell
guessTx :: PubKeyHash -> TxOutRef -> Value -> GameDatum -> BuiltinByteString -> Tx
guessTx pkh gameRef gameVal dat answer =
  mconcat
    [ spendScript gameScript gameRef (Guess answer) dat
    , payToKey pkh gameVal
    ]
```

We do two things:

* spend script with right datum and redeemer
* pay the prize back to us

To spend script we use function:

```haskell
spendScript :: IsValidator script 
  => script -> TxOutRef -> RedeemerType script -> DatumType script -> Tx
```

We provide validator definition, reference to the UTXO of the script, and its redeemer and datum type.

The next thing is that we want to take the prize. For that we create output that holds the prize and
protected by our own pub key. We do it with the function:

```haskell
payToKey :: PubKeyHash -> Value -> Tx
```

For V2 plutus (Babbage era and above) we can also use reference inputs.
They are inputs that we can only read datum and we don't need to spend them.
They came in two flavors to read inlined and hashed datum inputs:

```haskell
refInputInline :: TxOutRef -> Tx
refInputHash   :: ToData datum => TxOutRef -> datum -> Tx
```
Note that it's improtant to respect the way datum is stored and use corresponding 
type of reference input.


