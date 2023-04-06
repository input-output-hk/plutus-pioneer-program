# Typed validators

You might be familiar with the notion of `TypedValidator` from the library `plutus-ledger`
which comes from the repo `plutus-apps`. In practice `plutus-apps` is heavy-weight and 
often becomes obsolete. To mitigate it's shortcomings in this library we stay at the 
plutus level. We work with types that are supported by `plutus-ledger-api` and
don't use higher-level types from `plutus-ledger`.

Nonetheless it's great to have type-safety and watch out for which datums and redeemers are
applied to specific validators. For that the library `plutus-simple-model` uses 
lightweight wrappers to enforce types for datums and redeemers based on scripts. 
They are defined in the module `Plutus.Model.Validator`.

There are three types of typed valdiators:

* typed validators for scripts: 

  ```haskell
  newtype TypedValidator datum redeemer = TypedValidator (Versioned Validator)
  ```

* typed minting policies:

  ```haskell
  newtype TypedPolicy redeemer = TypedPolicy (Versioned MintingPolicy)
  ```

* typed stake validators:

  ```haskell
  newtype StakeValidator redeemer = TypedStake (Versioned StakeValidator)
  ```

Where `Versioned` attaches plutus language version to the entity.
Plutus has several versions of the language (at the moment they are V1 and V2).
We can tag entities with Language version by functions:

```haskell
toV1, toV2 :: a -> Versioned a
```

We use trick with phantom types to attach type info to the underlying script.
Also we have type classes that can extract the type info:

```haskell
type IsData a = (ToData a, FromData a)

class IsData (DatumType a) => HasDatum a where
  type DatumType a :: Type

class IsData (RedeemerType a) => HasRedeemer a where
  type RedeemerType a :: Type
```

Also we can extract language and underlying validator or hash:

```haskell
class HasLanguage a where
  getLanguage :: a -> C.Language
  -- ^ Get plutus language version

class HasValidator a where
  toValidator :: a -> Validator
  -- ^ Get internal validator

class HasValidatorHash a where
  toValidatorHash :: a -> ValidatorHash
  -- ^ Get internal validator
```

We have constraints synonyms for validator and validator hash based entities:

```haskell
type IsValidator a = (HasAddress a, HasDatum a, HasRedeemer a, HasLanguage a, HasValidator a)

type IsValidatorHash a = (HasAddress a, HasDatum a, HasRedeemer a, HasLanguage a, HasValidatorHash a)
```

In `plutus-ledger` we created a special tag for example `Game` and we 
instanciated similiar type class called `ValidatorTypes` to specify datum and redeemer types.

In `plutus-simple-model` we have instances of type class `IsValidator` for all typed scripts:
`TypedValidator`, `TypedPolciy` and `TypedStake`. Instead of defining a type tag we
just use it as type synonym for:

```haskell
type Game = TypedValidator GameDatum GameRedeemer
```

We can create validator with plutus or plutarch and wrap it to `TypedValidator`:

```haskell
gameScript :: Game
gameScript = TypedValidator $ toV1 $ mkValidatorScript $$(PlutusTx.compile [|| gameContract ||])
```

We have constructors to create typed validators for different versions of the Plutus.
To define which version we are going to use we import constructors from `Plutus.Model.V1`
or `Plutus.Model.V2`. We have constructors like `mkTypedValidator` or `mkTypedPolicy`
with the same names but internally they use corresponding language tag to
annotate the version of the language properly.

Let's imagine that we work with PlutusV1. Then we import:

```haskell
import Plutus.Model.V1
```

And we have alias `mkTypedValidator` which combines wrappers:

```haskell
gameScript = mkTypedValidator $$(PlutusTx.compile [|| gameContract ||])
```

Based on this `gameContract` should have type `BuiltinData -> BuiltinData -> BuiltinData -> ()`.
But in plutus development we used to write typed valdiators and then wrap them `mkTypedValidator`.

For plutus development there is function to make script defined in type-safe way 
to work on raw builtin data (it's also defined in corresponding `Plutus.Model.V1` or `.V2` module):

```haskell
toBuiltinValidator :: (UnsafeFromData datum, UnsafeFromData redeemer) 
  => (datum -> redeemer -> ScriptContext -> Bool)
  -> (BuiltinData -> BuiltinData -> BuiltinData -> ()) 
```

So in plutus development we can define validator:

```haskell
import Plutus.Model.V1

gameContract :: GameDatum -> GameRedeemer -> ScriptContext -> Bool
gameContract = ...

gameScript :: Game
gameScript = TypedValidator $ 
  mkValidatorScript $$(PlutusTx.compile [|| toBuiltinValidator gameContract ||])
```

Note that plutus onchain code should also use proper Plutus version.
Actually `toBuiltinValidator` won't work on wrong version.

Similiar functions are defined for minting policies and stake valdiators:

```haskell
mkTypedPolicy :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> TypedPolicy redeemer
mkTypedStake  :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> TypedStake redeemer

toBuiltinPolicy :: (UnsafeFromData redeemer)
  => (redeemer -> ScriptContext -> Bool) -> (BuiltinData -> BuiltinData -> ())

toBuiltinStake :: (UnsafeFromData redeemer)
  => (redeemer -> ScriptContext -> Bool) -> (BuiltinData -> BuiltinData -> ())
```

## Example of typed validator

Let's define a game of hash guessing. 

```haskell
newtype GameDatum = GuessHash Plutus.BuiltinByteString
  deriving (Eq)

newtype GameAct = Guess Plutus.BuiltinByteString

PlutusTx.unstableMakeIsData ''GameDatum
PlutusTx.unstableMakeIsData ''GameAct
```

We have some secret bytestring that is hashed with SHA256 and result of hashing
is open to everybody. If user can guess the origin user can take the value of the UTXO.

Let's define a contract for this logic:

```haskell
gameContract :: GameDatum -> GameAct -> ScriptContext -> Bool
gameContract (GuessHash h) (Guess answer) _ =
  Plutus.sha2_256 answer Plutus.== h
```

Let's compile the script and create `TypedValidator` for testing with our library:

```haskell
import Plutus.Model (toBuiltinValidator, TypedValidator, mkTypedValidator)

type Game = TypedValidator GameDatum GameAct

-- | The GeroGov validator script instance
gameScript :: Game
gameScript = mkTypedValidator $$(PlutusTx.compile [|| toBuiltinValidator gameContract ||])
```

We have to define it in separate module than the types definition otherwise GHC
complains on mixing compilation with QuasiQuotes with derivation of `UnsafeData` instances
with template haskell.

That's it! This is our contract that we are going to test.

## Pay to script by ValidatorHash

Sometimes we don't have the validator definition but we want to pay to specific script
by `ValidatorHash`. For that we can use the same function `payToScript`
but we should use `TypeValidatorHash`-wrapper instead of `TypedValidator`:

```haskell
newtype TypedValidatorHash datum redeemer = TypedValidatorHash (Versioned ValidatorHash)
```

In this case our script can be defined like this:

```haskell
gameScript :: Game
gameScript = TypedValidatorHash $ toV1 gameValHash
```

