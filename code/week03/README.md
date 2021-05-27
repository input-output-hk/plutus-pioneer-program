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
![](img/week03__00001.png)
![](img/week03__00002.png)
![](img/week03__00003.png)




