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

What should never happen under normal circumstances is that a validation script runs and then fails. This is because you can always run the validation under the same conditions in the wallet.





