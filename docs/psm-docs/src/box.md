# Box - a typed TxOut

Often when we work with scripts we need to read `TxOut` to get datum hash to
read the datum next and after that we specify how datum is updated on TX.

Enter the Box - typed `TxOut`. The `Box` is `TxOut` augmented with typed datum. 
We can read Box for the script with the function:

```haskell
boxAt :: (IsValidator script) => script -> Run [TxBox script]
```

It reads the typed box. We can use it like this: 

```haskell
gameBox <- head <$> boxAt @Game gameScript
```

Sometimes it's useful to read the box by NFT, since often scripts are identified by unque NFTs:

```haskell
nftAt :: (IsValidator script) => script -> Run (TxBox script)
nftAt tv = ...
```

So let's look at the box:

```haskell
-- | Typed txOut that contains decoded datum
data TxBox a = TxBox
  { txBoxRef   :: TxOutRef    -- ^ tx out reference
  , txBoxOut   :: TxOut       -- ^ tx out
  , txBoxDatum :: DatumType a -- ^ datum
  }

txBoxValue :: TxBox a -> Value
```

It has everything that `TxOut` has, but also we have our typed datum.
There are functions that provide typical script usage. 

We can just spend boxes as scripts:

```haskell
spendBox :: (IsValidator v) => 
  v -> RedeemerType v -> TxBox v -> Tx
spendBox tv redeemer box
```

Also we can use read only box with reference inputs:

```haskell
readBox :: TxBox v -> Tx
```

The most generic function is `modifyBox`:

```haskell
modifyBox :: (IsValidator v) 
  => v -> TxBox v -> RedeemerType v 
  -> (DatumType v -> DatumMode (DatumType v)) 
  -> (Value -> Value) 
  -> Tx
modifyBox tv box redeemer updateDatum updateValue
```

It specifies how we update the box datum and value. 
Also, often we use boxes as oracles:

```haskell
readOnlyBox :: (IsValidator v)
  => v -> TxBox v -> RedeemerType v -> Tx
```

It keeps the datum and value the same.


