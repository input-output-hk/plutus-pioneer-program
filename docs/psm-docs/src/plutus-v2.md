## Plutus V2 features

Here we give brief review of Plutus V2 features and show how to use them in the library.
Some of them were already explained in tutorial but it's good to list them here as a reference.
We can find working example for all features at the `test` directory of the repo.
See test suites under V2 directory:  `Suites.Plutus.Model.Script.V2.*`.

### Inlined datums

We can inline datum values into `TxOut`. For that we use `payToScript`
with the `InlineDatum` modifier (see test suite example `Suites.Plutus.Model.Script.V2.Test.Game`):

Function definition:

```haskell
data DatumMode a 
  = HashDatum a      -- ^ store datum hash in TxOut
  | InlineDatum a    -- ^ store inlined datum value in TxOut

payToScript :: (HasAddress script, HasDatum script) => 
  script -> DatumMode (DatumType script) -> Value -> Tx
```

Example from the code:

```
initGameTx :: UserSpend -> Value -> BuiltinByteString -> Tx
initGameTx usp val answer =
  mconcat
    [ userSpend usp
    , payToScript gameScript (InlineDatum $ GuessHash $ Plutus.sha2_256 answer) val
    ]
```

Here we store the game hash right in the `TxOut`. 

### Reference inputs

Reference inputs allow us to use read-only inputs that do not require
execution of any logic onchain. They are guaranteed to be constant during TX-evaluation.
It's sort of global values for TX validation.

We use two variants for `TxOut`s that store datums by hash and inlined:


```haskell
refInputInline :: TxOutRef -> Tx
refInputHash   :: ToData datum => TxOutRef -> datum -> Tx
```

Note that it's important to respect the way datum is stored and use the corresponding 
type of reference input.

See the usage at the example: `Suite.Plutus.Model.V2.Test.Oracle.Hashed` 
or `.Oracle.Inlined`

### Reference scripts

Reference scripts allow us to store common scripts for validation in the ledger
and thus we can omit script definition in the TX itself. This can greatly reduce the size of the TX
which can become an important optimization technique. 

Reference scripts are used in three stages:

* load script to ledger with: `loadRefScript`
* create `TxOut` that references that script with: `payToRef`
* spend `TxOut` guarded by reference with: `spendScriptRef`

Let's discuss those stages.

#### Load script for reference

we load script for reference with function:

```haskell
loadRefScript :: (IsValidator script) => script -> Value -> Tx
```

It uses script and value which is paid for script storage. 
The value should be enough to store the script with given size.
At the moment this check is not enforced by the library, so any amount of Ada is good enough.

We store no Datum alongside the script because normally validation will use
the datum from `TxOut` that references this script and update it.
But if we need that functionality to store constant datums we can use the function:

```haskell
loadRefScriptDatum :: 
     (IsValidator script) 
  => script -> DatumMode (DatumType script) -> Value -> Tx
```

For `DatumMode` explanation see Ininled datums section.

#### Create `TxOut` that references the script

To guard `TxOut` by referenced script we use the function:

```haskell
payToRef :: (IsValidator script) =>
  script -> DatumMode (DatumType script) -> Value -> Tx
```

It's the same as `payToScript` only it does not store the validator internally
thus reducing the TX-size. Actually the usage is the same as for `payToScript`.

#### Spend TxOut guarded by reference

To spend the script we use the function:

```haskell
spendScriptRef ::
  (IsValidator script) =>
  TxOutRef ->
  script ->
  TxOutRef ->
  RedeemerType script ->
  DatumType script ->
  Tx
spendScriptRef refToScript script refToUtxo redeemer datum
```

It's the same as `spendScript` only it has one additional first argument
that mentiones the UTXO that stores the validation script.

#### How to find the reference UTXO

We query normal UTXOs with functions `utxoAt` and `withUtxo`.
But for UTxOs that store reference scripts we are going to use special variants:
`refScriptAt` and `withRefScript`:

```haskell
refScriptAt :: HasAddress user => user -> Run [(TxOutRef, TxOut)]

withRefScript :: 
     HasAddress user 
  => ((TxOutRef, TxOut) -> Bool) -> user -> ((TxOutRef, TxOut) -> Run ()) -> Run ()
```

For ease of use regular UTxOs and reference script UTxOs are stored in different 
containers. Otherwise we need to filter on certain conditions to distinguish
reference script UTxO and UTxO that references it as they refer to the same script address.
This can quickly become annoying. 

See complete example of usage for reference scripts at `test`: `Suites.Plutus.Model.Script.V2.GameRef`.


