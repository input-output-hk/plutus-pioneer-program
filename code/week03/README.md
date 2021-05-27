# Week 03

For this lecture we will be working with a later commit of Plutus than in previous lectures. You will find the commit in the cabal.project file for Week03.

    cd /path/to/Plutus
    git checkout 3aa86304e9bfc425667051a8a94db73fcdc38878

It would, of course, be better for everyone if we could keep the Plutus dependencies stable, but this is not really possible as Plutus is evolving very quickly while heading towards the Alonzo release, where Plutus is fully integrated into the Cardano node.

If we wait too long and stay on an outdated version, then when we finally have to upgrade to use Plutus on the testnet, there will be lots of changes.

This does mean that some of the code from the first two lectures will not compile against the new version.

But, luckily, the changes are not that bad.

Let's take the last example from Week 02 and port it to the new Plutus version to see what has changed.

## Porting IsData

### Code Changes

The first difference is in the *mkValidator* function.

    mkValidator :: () -> MySillyRedeemer -> ScriptContext -> Bool
    mkValidator () (MySillyRedeemer r) _ = traceIfFalse "wrong redeemer" $ r == 42

In the previous version, the third argument was called *ValidatorCtx*. Luckily, we have not yet looked at this argument in detail.

The second change is where we create the scrAddress.

    scrAddress :: Ledger.Address
    scrAddress = scriptAddress validator

Previously, *scrAddress* was created using *ScriptAddress* (capital S), passing in a validator hash. This is because the address type has now changed in order to allow a component of the address relating to a staking address. But there is still a smart constructor *scriptAddress* (small s).

We don't need the validator hash anymore. It still exists and we could compute it, but we don't need it.

### Playground Changes

There have also been some changes in the playground.

One is a pleasant surprise. In previous lectures, we needed to remove the *module* header in the script after copy-pasting the code into the playground. We no longer need to do that.

Another interesting change is that fees are now considered in the playground. They are not yet realistic. The fees are always 10 lovelace, but in the real system the fees will depend on the memory consumption and the time it takes to execute the validators.

But, in any case, as there is now a fee of 10 lovelace, it no longer makes sense to have examples with such small balances in the wallets. So, instead of starting with 10 lovelace in each wallet, we should choose a bigger number, for example 1000.

Let's look at the changes in the playground.

![](img/week03__00000.png)

The Genesis transaction is the same, with the exception that the wallets are now given 1000 lovelace each, rather than 10.

![](img/week03__00001.png)

Now, we see the *give* transaction, with an addition fee output of 10 lovelace. This 10 lovelace has been deducted from the UTxO that represents the change of 300 (1000-700) for wallet 1.

![](img/week03__00002.png)

Now, the *grab*. Again, 10 lovelace in fees is deducted from the 700 lovelace that Wallet 2 grabbed.

![](img/week03__00003.png)

## Recap

When we explained the (E)UTxO model in Lecture One, we mentioned that in order to unlock a script address, the script attached to the address is run, and that script gets three pieces of information - the *Datum*, the *Redeemer* and the *Context*.

In the second lecture, we saw examples of that, and we saw how it actually works in Haskell. 

We saw the low-level implementation, where all three arguments are represented by the *Data* type. We also saw that in practice this is not used.

Instead, we use the typed version, where *Datum* and *Redeemer* can be custom types (as long as they implement the *IsData* type class), and where the third argument is of type *ScriptContext* (previously *ValidatorCtx*).

In the examples we have seen so far we have looked at the *Datum* and the *Redeemer*, but we have always ignored the *Context*. But the *Context* is, of course, very important. So, in this lecture we will start looking at the *Context*.

## ScriptContext

The *ScriptContext* type is defined in package *plutus-ledger-api*, which is a package that, until now, we haven't needed. But now we do need it, and it is included in this week's cabal.project file. It is defined in module *Plutus.V1.Ledger.Contexts*.

    data ScriptContext = ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }

It is a record type (a Haskell type where the fields are given names, rather than being referred to only by their position and type, although it can still be treated in such a manner if desired).

The second field is of type *ScriptPurpose*, which is defined in the same module. It defines for which purpose a script is being run. 

    -- | Purpose of the script that is currently running
    data ScriptPurpose
        = Minting CurrencySymbol
        | Spending TxOutRef
        | Rewarding StakingCredential
        | Certifying DCert

For us, the most important is *Spending*. This is what we have talked about so far in the context of the (E)UTxO model. This is when a script is run in order to validate a spending input for a transaction.

The *Minting* purpose comes into play when you want to define a native token. Its purpose us to describe under which cirumstances the native token can be minted or burned.

There are also two new brand new purposes - *Rewarding* - related to staking and *Certifying* - related to stake delegation.

The most interesting field is *scriptContextTxInfo* which is of type *TxInfo*, also defined in the same module.

    -- | A pending transaction. This is the view as seen by validator scripts, so some details are stripped out.
    data TxInfo = TxInfo
        { txInfoInputs      :: [TxInInfo] -- ^ Transaction inputs
        , txInfoInputsFees  :: [TxInInfo]     -- ^ Transaction inputs designated to pay fees
        , txInfoOutputs     :: [TxOut] -- ^ Transaction outputs
        , txInfoFee         :: Value -- ^ The fee paid by this transaction.
        , txInfoForge       :: Value -- ^ The 'Value' forged by this transaction.
        , txInfoDCert       :: [DCert] -- ^ Digests of certificates included in this transaction
        , txInfoWdrl        :: [(StakingCredential, Integer)] -- ^ Withdrawals
        , txInfoValidRange  :: SlotRange -- ^ The valid range for the transaction.
        , txInfoSignatories :: [PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
        , txInfoData        :: [(DatumHash, Datum)]
        , txInfoId          :: TxId
        -- ^ Hash of the pending transaction (excluding witnesses)
        } deriving (Generic)

It describes the spending transaction. In the (E)UTxO model, the context of validation is the spending transaction and its inputs and outputs. This context is expressed in the *TxInfo* type.

In particular you will see the list of all the inputs (*txInfoInputs*) and the list of all the outputs (*txInfoOutputs*), whose types provide a variety of fields to drill into each individual input or output.

We also see fields for fees *txFee*, the forge value *txInfoForge*, used when minting or burning native tokens.

The field *txInfoValidRange*, which we will come to in a moment, defines the slot range for which this transaction is valid.

The *txInfoData* field is a list associating *Datums* with their respective hashes.  If there is a transaction output to a script address that carries some *Datum*, you don't need to include the *Datum*, you can just include the *Datum* hash. However, you can optionally attach the *Datum*, in which case it will be done in the *txInfoData* list.

The *txInfoId* field is a hash of the transaction including all its inputs and outputs.

### txInfoValidRange

While there is a lot of information contained in this *txInfo* type, for our first example of how to use the third argument to validation, we will concentrate on the *txInfoValidRange* field.

This brings us to an interesting dilemma. We have stressed several times that the big advantage that Cardano has over something like Ethereum is that validation can happen in the wallet. But we have also noted that a transaction can still fail on-chain following  validation if, when the transaction arrives on the blockchain, it has been consumed already by someone else. In this case, the transaction fails without having to pay fees.

What should never happen under normal circumstances is that a validation script runs and then fails. This is because you can always run the validation under exactly the same conditions in the wallet, so it would fail before you ever submit it.

So that is a very nice feature, but it is not obvious how to manage time in that context. Time is important, because we want to be able to express that a certain transaction is only valid before or only valid after a certain time has been reached.

We saw an example of this in lecture one - the auction example, where bids are only allowed until the deadline has been reached, and the *close* endpoint can only be called after the deadline has passed.

That seems to be a contractiction, because time is obviously flowing. So, when you try to validate a transaction that you are constructing in your wallet, the time that you are doing that can, of course, be different than the time that the transaction arrives at a node for validation. So, it's not clear how to bring these two together so that validation is deterministic, and to guaranteee that if, and only if, validation succeeds in the wallet, it will also succeeed at the node.

The way Cardano solves that, is by adding the slot range field *txInfoValidRange* to a transaction, which essentially says "This transaction is valid between *this* and *that* slot". 

When a transaction gets submitted to the blockchain and validated by a node, then before any scripts are run, some general checks are made, for example that all inputs are present and that the balances add up, that the fees are included and so on. One of those checks is to check that the slot range is valid.

This means that we are completely deterministic again because if the script is run, we know that we are within the valid slot range.

By default, a script will use the infinite slot range, one that covers all slots until the end of time, but we do have the option to set a different slot range, and that is what we have to do if we deal with time-critical smart contracts, like in the auction example.

So, let's look at this slot range type in more detail.

### Slot

One relevant module, found in package *plutus-ledger-api* is:

    Plutus.V1.Ledger.Slot

When we look at the file in which *Slot* is defined, we see that it is a type wrapper around *Integer*.

    -- | The slot number. This is a good proxy for time, since on the Cardano blockchain
    -- slots pass at a constant rate.
    newtype Slot = Slot { getSlot :: Integer }
        deriving stock (Haskell.Eq, Haskell.Ord, Show, Generic)
        deriving anyclass (FromJSON, FromJSONKey, ToJSON, ToJSONKey, NFData)
        deriving newtype (Haskell.Num, AdditiveSemigroup, AdditiveMonoid, AdditiveGroup, Enum, Eq, Ord, Real, Integral, Serialise, Hashable, PlutusTx.IsData)
        
In order to construct a value of type *Slot*, we can use the *Slot* constructor, but it's even easier if you look at the implemented type classes, where we can see that it also implements the *Num* type class, which means that we can use numeric literals, so we can simply write 17, for example, rather than "Slot 17", or "Slot {getSlot=17}".

An example from the REPL:

    Prelude Week03.IsData> import Plutus.V1.Ledger.Slot 

    Prelude Plutus.V1.Ledger.Slot Week03.IsData> Slot 17
    Slot {getSlot = 17}

    Prelude Plutus.V1.Ledger.Slot Week03.IsData> Slot {getSlot=17}
    Slot {getSlot = 17}

    Prelude Plutus.V1.Ledger.Slot Week03.IsData> 17 :: Slot
    Slot {getSlot = 17}

The definition of *SlotRange* is

    -- | An 'Interval' of 'Slot's.
    type SlotRange = Interval Slot

So *SlotRange* is an *Interval Slot* - so what is *Interval*? That is defined in a module in the same package - *plutus-ledger-api*.

This is more general and is not necessarily for *Slot*s. Here, we are only concerned with the case where the type variable *a* is *Slot*.

    --   The interval can also be unbounded on either side.
    data Interval a = Interval { ivFrom :: LowerBound a, ivTo :: UpperBound a }
        deriving stock (Haskell.Eq, Haskell.Ord, Show, Generic)
        deriving anyclass (FromJSON, ToJSON, Serialise, Hashable, NFData)

There are some slight complications. For example, you can specify whether one or both of the bounds are inclusive, and you have the special case where the upper bound is infinity and the case where the lower bound is the beginning of time.

Normally, we don't have to deal with types directly because we have nice helper functions. The most general of these helper functions is probably the *interval* function, which takes an inclusive lower bound and an inclusive upper bound and constructs an interval from those values.

The comment on this function in the commit we are working with in this lecture is incorrect - it claims that the upper bound is not inclusive, but it actually is.

    interval :: a -> a -> Interval a
    interval s s' = Interval (lowerBound s) (upperBound s')

There is also the *singleton* helper, which constructs an interval which consists of just one slot.

    singleton :: a -> Interval a
    singleton s = interval s s

We have *from* which constructs an *Interval* starting from a given slot and extending to the end of time.

    from :: a -> Interval a
    from s = Interval (lowerBound s) (UpperBound PosInf True)

And we have *to*, which is the opposite. It constructs an *Interval* starting from the genesis block up to, and including, the given slot. Again, the comments in the code for the commit we are working with claims that it is not inclusive, but it is.

    to :: a -> Interval a
    to s = Interval (LowerBound NegInf True) (upperBound s)

We have *always* which contains all slots from the beginning of time until the end of eternity. This is the default.

    always :: Interval a
    always = Interval (LowerBound NegInf True) (UpperBound PosInf True)

And we have the opposite, *never*, which contains no slots.

    never :: Interval a
    never = Interval (LowerBound PosInf True) (UpperBound NegInf True)

In addition to these helper functions for constructing values of type *Interval*, we have various helpers for working with *Interval*s.

The *member* function checks whether a value is contained within an *Interval*.

    member :: Ord a => a -> Interval a -> Bool
    member a i = i `contains` singleton a

The *overlaps* function checks whether two intervals overlap, that is, whether there is a value that is a member of both intervals.

    overlaps :: Ord a => Interval a -> Interval a -> Bool
    overlaps l r = isEmpty (l `intersection` r)

The *intersection* function determines the largest interval that is contained in both the given intervals. This is an *Interval* that starts from the largest lower bound of the two intervals and extends until the smallest upper bound.

    intersection :: Ord a => Interval a -> Interval a -> Interval a
    intersection (Interval l1 h1) (Interval l2 h2) = Interval (max l1 l2) (min h1 h2)

The function *hull* gives the smallest interval containing both the given intervals.

    hull :: Ord a => Interval a -> Interval a -> Interval a
    hull (Interval l1 h1) (Interval l2 h2) = Interval (min l1 l2) (max h1 h2)

The *contains* function takes two intervals and determines if the second interval is completely contained within the first one.

    contains :: Ord a => Interval a -> Interval a -> Bool
    contains (Interval l1 h1) (Interval l2 h2) = l1 <= l2 && h2 <= h1

And we have the *before* and *after* functions to determine if a given *Slot* is before or after a given *Interval*, respectively.

    before :: Ord a => a -> Interval a -> Bool
    before h (Interval f _) = lowerBound h < f

    after :: Ord a => a -> Interval a -> Bool
    after h (Interval _ t) = upperBound h > t

Let's have a play in the REPL.

