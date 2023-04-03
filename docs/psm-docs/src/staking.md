# Staking and certificates

## How to pay to the addresses with staking credentials

We can append the information on staking credential to
anything that is convertible to `Address` by `HasAddress` type class
with constructor `AppendStaking`. Also, we have utility functions
`appendStakingPubKey` and `appendStakingScript` which
append `PubKeyHash` and `ValidatorHash` as staking credential.

For example, we can append it to the `TypeValidator` of the script:

```haskell
payToScript (appendStakingPubKey stakingKey typedValidator) datum value
```

That's why we use not `TypedValidator` in the payToScript and similiar functions.
It turns out that if `v` is `IsValidator` and `HasAddress` then `AppendStaking v` 
is also `IsValidator` and `HasAddress` and correct datum and redeemer types are set up.

So we can use the same functions with scritps that have some staking info attached to them.
Let's recall the type:

```haskell
payToScript :: (HasAddress v, HasDatum v) => v -> DatumType v -> Value -> Tx
```

The same function exists to pay to pub key hash:

```haskell
payToKey :: HasAddress pubKeyHash => pubKeyHash -> Value -> Tx
```

## Certificates and withdrawals of the rewards

In cardano staking pools are driving force behind confirmation of transactions.
For the work pool owners receive fees. The fees get distributed among 
staking credentials that delegate to pool. 

So we have staking pool operator (SPO) who is chosen for this round to
confirm TX. And for this work SPO gets the fees. Fees are collected 
to the addresses that are called staking credentials. Users can delegate staking credential
to the pool owner (SPO). 

So far we have ignored the fees, but in real scenario every transaction should contain fees.
To pay fee in the transaction we can use function. The fees are fairly distributed among
all staking credentials that belong to the pool:

```haskell
-- Pay fee for TX confirmation. The fees are payed in ADA (Lovelace)
payFee :: Ada -> Tx
```
The `Ada` is a newtpye wrapper over `Integer` that specifies amount of lovelaces.
It is defined in the module `Plutus.Model.Ledger.Ada`.



