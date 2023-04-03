# Human-readable names

Often various entities are expressed as hashes on Cardano/Plutus, 
i.e `CurrencySymbol`, `PubKeyHash`, `ValidatorHash`, `TxId` etc. 
Sometimes it's hard to distinguish between those hashes and on error messages we would like to
see more meaningful names. 

We can assign such names to hashes with family of functions:

```haskell
writeUserName           :: PubKeyHash     -> String -> Run ()
writeAddressName        :: Address        -> String -> Run ()
writeAssetClassName     :: AssetClass     -> String -> Run ()
writeCurrencySymbolName :: CurrencySymbol -> String -> Run ()
writeTxName             :: Tx             -> String -> Run ()
```

they assign names to various hash based entities and those names would be
printed on error logs. Also we can query those names with functions:

```haskell
getPrettyAddress        :: HasAddress user => user -> Run String
getPrettyCurrencySymbol :: CurrencySymbol          -> Run String
getPrettyAssetClass     :: AssetClass              -> Run String
getPrettyTxId           :: TxId                    -> Run String
```

We can use those functions in combo with `logError` or `logInfo` to
give meaningful information to echo-prints or error logs.

It's good to write the names on the initial setup stage for the application.
So that all next error messages could benifit from those names.


