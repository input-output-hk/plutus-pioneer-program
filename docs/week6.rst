Week 06 - Oracles
=================

.. note::
      These is a written version of `Lecture
      #6 <https://www.youtube.com/watch?v=Tr2VBm8vOhw>`__.

      In this lecture we learn about oracles and using the PAB (Plutus Application Backend).

      These notes use Plutus commit 476409eaee94141e2fe076a7821fc2fcdec5dfcb

      
Overview
--------

In this lecture we are going to look at a case study, to see how what we have learned so far can be turned into an actual application. A collection of executables that even come with a little front end.

It will be a real dApp, apart from the fact that we don't have a real blockchain available yet. This will run on a
simulated blockchain - a mockchain.

The example we are going to use for this is to implement a very simple oracle.

.. note::
    In the blockchain world, an oracle is a way to get real-world information onto the blockchain, in order to
    make it usable in smart contracts.

There are numerous examples of use cases for oracles. We can think of external data sources such as weather data, election
results, stock exchange data or randomness. You may have a betting contract that depends on the outcome of a specific
sports game, for example.

There are various ways to implement oracles, of varying sophistication.

We are going to use a very simple approach, where we have one trusted data provider. And, as an example of data, we
are going to use the ADA/USD exchange rate.

There are lots of problems with this approach, as we have to trust the data source. There are ways to mitigate the risk
that the data source is either untrustworthy or unreliable. For example, we could ask the provider to put down some
collateral that is lost if data is not provided or is inaccurate. Or, you could combine several oracles into One
and only accept the result if they all agree, or take the median, or average value of various sources. You could also
come up with more sophisticated mechanisms.

As we know, for anything to happen on the blockchain, there must be a UTxO, so the obvious thing to do is to represent
the data feed as a UTxO. The UTxO sits at the script address of the oracle, and its datum field it carries the current 
value of the oracle data.

.. figure:: img/week06__00000.png

And this is where we find our first problem. As we have noted before, validation only happens when you want to consume
something from a script address, not when you produce an output at a script address. This means that we can't prevent
anybody producing arbitrary outputs at the script address.

.. figure:: img/week06__00001.png

Somehow we need to distinguish the true oracle output from other outputs that may be sitting at the same script address.
And the way we do this is to put an NFT on the output. Because an NFT can only exist once, there can only be one UTxO
at the script address that holds the NFT.

.. figure:: img/week06__00002.png

How can such an oracle be used?

Here we come to something we haven't seen before. In all our code writing validators and contracts, we always knew the
full API up front. In the case of an oracle, this is different. At the point that an oracle is created, you don't know how
people may want to use it. It must be like an open API, able to work with smart contracts that have not yet been
designed.

As an example of a use-case that might make use of this specific oracle, let's consider a swap contract where, at the swap
address, somebody can deposit ADA, and then somebody else can take those ADA in exchange for USD.

.. figure:: img/week06__00003.png

Of course, we don't have USD directly on the blockchain, but we can imagine that they are represented by some native token.

In this example, as the value at the oracle is 1.75, then if someone offers 100 ADA, the price for that should be
175 USD.

In addition to this, we need an incentive for the oracle to provide the data, because in additional to other costs for
providing the data, at a minimum they would have to pay fees to create the UTxO.

So, let's say that the oracle provider determines a fee of 1 ADA that has to be paid each time the oracle is used.

In this example, that would mean that the person wanting to by the ADA would have to pay 175 USD to the seller of the ADA,
and 1 ADA to the oracle.

What will the transaction look like?

.. figure:: img/week06__00004.png

First of all, the swap validation logic will need access to the current oracle value, which means that the oracle UTxO must be an input to the transaction.

Then we have the oracle validation logic. In this case we want to use the oracle. So, let's say we have a redeemer called *use*. Now, the oracle validator has
to check several things.

1. Is the NFT present in the consumed input?
2. Is there an output from the transaction at the same address containing the same NFT?
3. Is the value in the output UTxO the same as the input value?
4. Is the fee present?

Now we can complete the transaction.

We consume two additional inputs - the fee paid by the buyer and the 100 ADA deposited by the seller. Then we have two additional outputs - the 175 USD to the seller, and the 100 ADA to the buyer. And for these new inputs and
outputs, it is the responsibility of the swap validator to make sure that it is correct. Whereas, the oracle validator is only interested with making sure that everything concerning the oracle is correct.

.. figure:: img/week06__00005.png

Just to emphasize, this swap contract is just an example. The oracle should be capable of working with many different smart contracts that want to make use of its data.

If this was all, then we wouldn't need an oracle. If the value was fixed, so that it was always 1.75 then we could simply hard-code this into our contract. So, the value must be able to change. At least, in an example such
as this one where we have an exchange rate that can, of course, change over time. There may be other examples such as the result of a sports match, where it is a singular event in history, but in this case, it is important 
that it be able to change.

This means that the oracle validator, in addition to the *use* redeemer, must be able to support another operation where the provider of the oracle can actually change the data.

So let's say the the value changes from 1.75 to 1.77.

We know that on a (E)UTxO blockchain, nothing ever changes, so you can't change the datum of an existing UTxO. All you can do is consume UTxOs and produce new ones.

We would have a transaction that uses an *update* redeemer. The validation logic is somewhat different. It is the same as before in that the NFT needs to be present in the consumed oracle input, and also needs to be
present in the new output. In addition to that, the transaction must be signed by the oracle provider. And, we can use this update transaction as an opportunity for the oracle provider to collect the fees.

We insist that the NFT be present in the output, but we don't say anything about other values. All the fees that got there by other transactions using this oracle data can be collected during the *update* transaction.

.. figure:: img/week06__00006.png

Summary
~~~~~~~

To sum up, we represent the oracle by a UTxO and identify the correct UTxO with an NFT. The oracle value is the datum of the UTxO. We support two operations. 

One is *use* which uses the oracle in some arbitrary 
transaction. The *use* validator will make sure that the consumed oracle input carries the NFT, that there is an output that again carries the NFT, doesn't change the datum, and carries additional fees.

The second operation is *update* which can only be done by the oracle provider. For an *update* transaction, the oracle input must again carry the NFT, there must be an oracle output, also carrying the NFT. There are no 
further restrictions. The datum can change, and the accumulated fees can be taken out.

Oracle Core
-----------

Now that we know how it is supposed to work, let's look at some code.

On-chain
~~~~~~~~

First, let's look at the Plutus code that implements the oracle itself.

.. code:: haskell

    module Week06.Oracle.Core

The oracle will be a parameterized contract, and it will depend on four fields.

.. code:: haskell

    data Oracle = Oracle
        { oSymbol   :: !CurrencySymbol
        , oOperator :: !PubKeyHash
        , oFee      :: !Integer
        , oAsset    :: !AssetClass
        } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)    

- *oSymbol* is the currencySymbol of the NFT that is used to identify the transaction. We don't need the token name as we will just use the empty string as the token name.
- *oOperator* is the owner of the oracle - the hash of the public key owner which can make updates
- *oFee* is the fee in lovelace that is due every time the oracle is used
- *oAsset* represents the asset class that we want to exchange rate for against Ada, which in our case will be some kind of USD token

The redeemer will support two operations.

.. code:: haskell

    data OracleRedeemer = Update | Use
        deriving Show

    PlutusTx.unstableMakeIsData ''OracleRedeemer

We need to define the NFT asset class. As mentioned, we are going to use the empty string for the token name.

.. code:: haskell

    {-# INLINABLE oracleTokenName #-}
    oracleTokenName :: TokenName
    oracleTokenName = TokenName emptyByteString
    
The *oracleAsset* will be used to identify the NFT - this is not to be confused with *oAsset*, defined above.

.. code:: haskell

    {-# INLINABLE oracleAsset #-}
    oracleAsset :: Oracle -> AssetClass
    oracleAsset oracle = AssetClass (oSymbol oracle, oracleTokenName)

We create a little helper function called *oracleValue*. This takes an output transaction and a function which looks up the datum, and then returns an *Integer*. The *Integer* represents the exchange rate (e.g. 1.75) multiplied
by a million. This avoids potential complications when using real numbers.

.. code:: haskell

    {-# INLINABLE oracleValue #-}
    oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer
    oracleValue o f = do
        dh      <- txOutDatum o
        Datum d <- f dh
        PlutusTx.fromData d
        
This function is an example of monadic computation in monad that is not *IO* or the *Contract* monad. First we call *txOutDatum*, which can fail if because not every output has a datum. If it succeeds, we get a datum hash
which we can reference in *dh*. Next we used the function *f* which is provided as the second argument to maybe turn this datum hash into a datum. This too can fail. If it succeeds we can reference the result in *d*. *Datum*
is just a newtype wrapper around *Data*, so we can then use *PlutusTx.fromData* to maybe turn *d* into an *Integer*. Again, this can fail, because even if the datum is there, it may not be convertible to an integer value.

We will see in a moment where we use the *oracleValue* function.

The most important function is *mkOracleValidator*.

.. code:: haskell

    {-# INLINABLE mkOracleValidator #-}
    mkOracleValidator :: Oracle -> Integer -> OracleRedeemer -> ScriptContext -> Bool
    mkOracleValidator oracle x r ctx =
        traceIfFalse "token missing from input"  inputHasToken  &&
        traceIfFalse "token missing from output" outputHasToken &&
        case r of
            Update -> traceIfFalse "operator signature missing" (txSignedBy info $ oOperator oracle) &&
                      traceIfFalse "invalid output datum"       validOutputDatum
            Use    -> traceIfFalse "oracle value changed"       (outputDatum == Just x)              &&
                      traceIfFalse "fees not paid"              feesPaid
      where
        info :: TxInfo
        info = scriptContextTxInfo ctx
    
        ownInput :: TxOut
        ownInput = case findOwnInput ctx of
            Nothing -> traceError "oracle input missing"
            Just i  -> txInInfoResolved i
    
        inputHasToken :: Bool
        inputHasToken = assetClassValueOf (txOutValue ownInput) (oracleAsset oracle) == 1
    
        ownOutput :: TxOut
        ownOutput = case getContinuingOutputs ctx of
            [o] -> o
            _   -> traceError "expected exactly one oracle output"
    
        outputHasToken :: Bool
        outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleAsset oracle) == 1
    
        outputDatum :: Maybe Integer
        outputDatum = oracleValue ownOutput (`findDatum` info)
    
        validOutputDatum :: Bool
        validOutputDatum = isJust outputDatum
    
        feesPaid :: Bool
        feesPaid =
          let
            inVal  = txOutValue ownInput
            outVal = txOutValue ownOutput
          in
            outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))
            
            
The function *mkOracleValidator* takes our parameter *Oracle*, the datum, which, in this example is an *Integer*, the redeemer type *OracleRedeemer* and finally the *ScriptContext*.

There are two cases for this validator - *use* and *update* - but there are similarities. In both cases we want to check that we have the input that holds the NFT and that there is an output that holds the NFT.

As both these checks need to be done regardless of the use case, they are done upfront.

.. code:: haskell

    ...
    traceIfFalse "token missing from input"  inputHasToken  &&
    traceIfFalse "token missing from output" outputHasToken &&
    ...    
    
After this, we consider which use case we are dealing with.

.. code:: haskell

    case r of
        Update -> traceIfFalse "operator signature missing" (txSignedBy info $ oOperator oracle) &&
                  traceIfFalse "invalid output datum"       validOutputDatum
        Use    -> traceIfFalse "oracle value changed"       (outputDatum == Just x)              &&
                  traceIfFalse "fees not paid"              feesPaid    

Before looking at the *inputHasToken* function there is another help function to look at.

.. code:: haskell

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i
        
The *ownInput* function returns the *TxOut* that the script is trying to consume, which in this case is the oracle output. The *Nothing* case here can happen if we are in a 
different context, such as a minting context, so this eventuality will not occur for us. The *findOwnInput* function is provided by Plutus and will, given the context, 
find the relevant input. The *txInInfoResolved* function gets the *TxOut* from the *TxInInfo*.

The *inputHashToken* function checks that the token is present. It uses the *assetClassValueOf* function to look for the NFT within the *ownInput* response.

.. code:: haskell

    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oracleAsset oracle) == 1

The next helper function, *ownOutput* checks that we have exactly one output and returns that output to us.

.. code:: haskell

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle output"    

We can use this for the *outputHasToken* helper function in the same way as we did for the *inputHashToken* function.

.. code:: haskell

    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleAsset oracle) == 1

That covers the code for the common cases. Now, let's let at the code specific to the *update* case.

There are two conditions to check. The first is that the operator actually signed the transaction. This is so simple that we can do it inline without
a helper function.

.. code:: haskell

    traceIfFalse "operator signature missing" (txSignedBy info $ oOperator oracle)

The next thing to check is that the output datum. We know that the value can change, but we need to check that it is at least of the correct type.

.. code:: haskell

    traceIfFalse "invalid output datum" validOutputDatum
    
And for this we have referenced a new helper function *validOutputDatum*, which itself makes use of a helper function *outputDatum*.

.. code:: haskell

    outputDatum :: Maybe Integer
    outputDatum = oracleValue ownOutput (`findDatum` info)    

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatum

.. note::
    If you look up *findDatum* in the REPL, you will see it has a type of *DatumHash -> TxInfo -> Maybe Datum*. As we are using its infix notation here, we
    can pass in *info* as the only parameter, and this will result in the whole expression having the type *DatumHash -> Maybe Datum*, which is the type we
    need to pass into *oracleValue*.
    
This works by trying to get the datum value from the datum hash and then trying to create the oracle value from it. If it succeeds it will return a *Just Integer*,
otherwise it will return *Nothing*, so the *validOutputDatum* function just needs to check that the return value is not *Nothing*, in other words, that it 
is a *Just*.

Note that we are not checking anything about the value of the *Integer*. This could even remain the same as the input value, if the transaction is used 
just to collect the fees that have accumulated from the use the oracle.

The second case for *mkOracleValidator* is the *use* case. This case can be used by anyone, but it is much more restrictive.

First, we don't allow the value to change. So this is the first condition.

.. code:: haskell

    traceIfFalse "oracle value changed" (outputDatum == Just x)

We have already written the *outputDatum* helper function. Instead of checking only that it is an *Integer*, here we also check that its output value is the same
as the input value.

And finally, we must check that the fees have been paid. And for this we use a new helper function called *feesPaid*.

.. code:: haskell

    feesPaid :: Bool
    feesPaid =
      let
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
      in
        outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))    

This *feesPaid* function checks that the output value is at least as much as the input value plus the required fee. We again use the semigroup operator 
*<>* to add the fee value to the input value. We could have used equal (eq) instead of greater than or equal (geq). Using *geq* allows the user of the 
oracle to give the oracle provider a tip, if they so wish.

So this now is basically the core business logic of the oracle as shown in the diagrams.

.. figure:: img/week06__00006.png

Now we have our usual boilerplate. In particular notice that we use the pattern that we need for a parameterized validator.

.. code:: haskell

    data Oracling
    instance Scripts.ScriptType Oracling where
        type instance DatumType Oracling = Integer
        type instance RedeemerType Oracling = OracleRedeemer

    oracleInst :: Oracle -> Scripts.ScriptInstance Oracling
    oracleInst oracle = Scripts.validator @Oracling
        ($$(PlutusTx.compile [|| mkOracleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
        $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @Integer @OracleRedeemer

    oracleValidator :: Oracle -> Validator
    oracleValidator = Scripts.validatorScript . oracleInst

    oracleAddress :: Oracle -> Ledger.Address
    oracleAddress = scriptAddress . oracleValidator

And this concludes the on-chain part of the oracle code.
    
Off-chain
~~~~~~~~~

We also create some off-chain code, namely to start the oracle, and to update it. However, we don't write off-chain code to *use* the oracle. That is not the 
responsibility of the author of this contract. That will be the responsibility of the person that wants to use the oracle - they will write the code to create the
transaction with the *use* redeemer. This is the first time that we have seen the situation where we have some on-chain code that is not paired with some off-chain code.

Starting the Oracle
+++++++++++++++++++

To start the oracle, we need some parameters.

.. code:: haskell

    data OracleParams = OracleParams
        { opFees   :: !Integer
        , opSymbol :: !CurrencySymbol
        , opToken  :: !TokenName
        } deriving (Show, Generic, FromJSON, ToJSON)    

The *opFees* parameter represents the number of lovelace that will be charged to use the oracle.

The *opSymbol* and *opToken* parameters represent the token against which we are providing the Ada exchange rate, in this case a USD token.

First we create a *startOracle* function, whose responsibility is to mint the NFT that will be used to identify the oracle UTxO. The *startOracle* function will
not provide an initial value for the oracle, this will be handled by the *updateOracle* function. The reason for this is that, if we provided an initial value, it may
be outdated by the time the NFT is minted.

We could have used the same code for minting the NFT as we used in lecture 5. This would have worked perfectly well.

However, this is a currency module provided in *plutus-use-cases* that provides a *forgeContract* function that allows us to mint NFTs.

Here is the type of the *forgeContract* function shown in the REPL.

.. code::

    Prelude Week06.Oracle.Core> :t Plutus.Contracts.Currency.forgeContract
    Plutus.Contracts.Currency.forgeContract
      :: (row-types-1.0.1.0:Data.Row.Internal.AllUniqueLabels
            (Plutus.Contract.Schema.Input s),
          row-types-1.0.1.0:Data.Row.Internal.AllUniqueLabels
            (Plutus.Contract.Schema.Output s),
          Plutus.Contracts.Currency.AsCurrencyError e,
          (Plutus.Contract.Schema.Input s
           row-types-1.0.1.0:Data.Row.Internal..! "tx-confirmation")
          ~ Plutus.Contract.Effects.AwaitTxConfirmed.TxConfirmed,
          (Plutus.Contract.Schema.Input s
           row-types-1.0.1.0:Data.Row.Internal..! "tx")
          ~ Plutus.Contract.Effects.WriteTx.WriteTxResponse,
          (Plutus.Contract.Schema.Output s
           row-types-1.0.1.0:Data.Row.Internal..! "tx")
          ~ Ledger.Constraints.OffChain.UnbalancedTx,
          (Plutus.Contract.Schema.Output s
           row-types-1.0.1.0:Data.Row.Internal..! "tx-confirmation")
          ~ Plutus.V1.Ledger.TxId.TxId) =>
         Plutus.V1.Ledger.Crypto.PubKeyHash
         -> [(Plutus.V1.Ledger.Value.TokenName, Integer)]
         -> Plutus.Contract.Types.Contract
              w s e Plutus.Contracts.Currency.OneShotCurrency
    
The important part starts towards the end, where the first parameter - of type *PubKeyHash* - is defined. This is the hash of the public key of the recipient of the NFT.

The *forgeContract* function provides more general functionality than our previous NFT contract. It allows is to generate multiple NFTs in one go. It will create a currency symbol that can only be used one, similar 
to our NFT from last time, so there can only be one minting transaction. But for the one currency symbol, you can mint various tokens in the same transaction, with 
various token names and in various quantities. The second parameter allows us to define these token names and quantities.

And it gives us a *Contract* that returns a value of the *OneShotCurrency* type. This type is specific to the currency and it doesn't really matter to us what it is. All that matters 
for us is that we can get the currency symbol out of it again.

There is one slight problem. This is not compatible with what we want. We want this types

.. code:: haskell

    Contract w s Text Oracle

An arbitrary writer type (because we don't make use of it), an arbitrary schema (as long as we have *BlockChainActions* available), *Text* error messages and a return 
type of *Oracle*.

The problem is that the *Contract* returned by *forgeContract* doesn't allow *Text* error messages. You can see this in the verbose output from the REPL - there is 
a constraint on the *e* parameter.

.. code:: haskell

    Plutus.Contracts.Currency.AsCurrencyError e,

Unfortunately *Text* doesn't implement *AsCurrencyError*.

Luckily there is a function that can helper

.. code:: haskell

    Plutus.Contract.mapError
    :: (e -> e')
       -> Plutus.Contract.Types.Contract w s e a
       -> Plutus.Contract.Types.Contract w s e' a

Given a *Contract*, it allows us to create a new *Contract* with a new type of error message. That is provided we give a function that converts from the first error 
type to the second error type.

So, let's look at the *startOracle* function.

.. code:: haskell

    startOracle :: forall w s. HasBlockchainActions s => OracleParams -> Contract w s Text Oracle
    startOracle op = do
        pkh <- pubKeyHash <$> Contract.ownPubKey
        osc <- mapError (pack . show) (forgeContract pkh [(oracleTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency)
        let cs     = Currency.currencySymbol osc
            oracle = Oracle
                { oSymbol   = cs
                , oOperator = pkh
                , oFee      = opFees op
                , oAsset    = AssetClass (opSymbol op, opToken op)
                }
        logInfo @String $ "started oracle " ++ show oracle
        return oracle
        
Here we see the error conversion function is provided as *pack . show*. The *show* function converts the error to a *String* and the *pack* function converts a *String*
to a *Data.Text* type.

At this point, *osc* holds the *OneShotCurrency*, and we can then use the *currencySymbol* function to get the currency symbol as *cs*.

The *currencySymbol* function has type

.. code:: haskell
    
    currencySymbol
          :: OneShotCurrency -> Plutus.V1.Ledger.Value.CurrencySymbol

And is used accordingly

.. code:: haskell

    let cs = Currency.currencySymbol osc
    
Now we have minted our NFT and it has currency symbol *cs*. And now we can construct our *Oracle* parameter value.

.. code:: haskell

    oracle = Oracle
        { oSymbol   = cs
        , oOperator = pkh
        , oFee      = opFees op
        , oAsset    = AssetClass (opSymbol op, opToken op)
        }

The reason that *opSymbol* and *opToken* are defined separately in the *OracleParams* type *op* is just that this makes is easier when we are using the playground.

Updating the Oracle
+++++++++++++++++++

The *updateOracle* function is more complicated.

This function has to deal with two cases. Namely, the case where we have a value that we wish to update, and the case where we have just started the oracle and we want
to create a value for the very first time.

It takes our oracle parameters and also the *Integer* value that we wish to have the oracle hold.

First we create a helper function *findOracle*.

.. code:: haskell

    findOracle :: forall w s. HasBlockchainActions s => Oracle -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Integer))
    findOracle oracle = do
        utxos <- Map.filter f <$> utxoAt (oracleAddress oracle)
        return $ case Map.toList utxos of
            [(oref, o)] -> do
                x <- oracleValue (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o
                return (oref, o, x)
            _           -> Nothing
      where
        f :: TxOutTx -> Bool
        f o = assetClassValueOf (txOutValue $ txOutTxOut o) (oracleAsset oracle) == 1
        
The purpose of *findOracle* is to look up the existing oracle UTxO. This can fail because the oracle might not be there. This will happen if we have just started the
oracles and have not yet created a UTxO with the oracle value. But, if we find it, we return a triple containing the UTxO identifer (TxOutRef), the UTxO itself, which 
contains all the data (TxOutTx) and the oracle value (the current exchange rate held by the oracle). The *Integer* containing the oracle value is encoded also in the
TxOutTx value, but we add it to the triple to make it easier to work with.

The first thing we do is to get all the UTxOs sitting at this address. But only one of these will be the one we are looking for - the one that contains the NFT.

We do this by using the *Map.filter* function which takes a function as a parameter which, in this case, returns True for the UTxO where the NFT is present.

.. code:: haskell

    utxos <- Map.filter f <$> utxoAt (oracleAddress oracle)
    ...
    where
      f :: TxOutTx -> Bool
      f o = assetClassValueOf (txOutValue $ txOutTxOut o) (oracleAsset oracle) == 1    

We will end up with a map in *utxos* which is either empty or contains one item. Now, we distinguish between these two cases.

.. code:: haskell

    return $ case Map.toList utxos of
        [(oref, o)] -> do
            x <- oracleValue (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o
            return (oref, o, x)
        _           -> Nothing
        
We convert the map to a list of tuples representing key value pairs of transaction ids and the transactions themselves.

For the case where there is no element, we use the _ case to represent all other cases. This could only ever be the empty list, but the compiler doesn't know that.

If, however, we have found the UTxO, then, as we already have its id and transaction, we just need to find its *Integer* value. This part could still go wrong. Even 
though we have found the correct UTxO, there could be some corrupt data in it for whatever reason.

We use the *oracleValue* function that we used also in validation. This function takes a *TxOut* parameter followed by a second parameter is a function, which, given a datum hash will return the associated datum.

In the off-chain code, we can use the following function parameter

.. code:: haskell

    \dh -> Map.lookup dh $ txData $ txOutTxTx o

Here, *txData* is a field of the transaction and it is a map from datum hashes to datums. We get the transaction from *txOutTxTx o*.

If this all succeeds, when will return the triple (oref, o, x), where x is the *Integer* value of the oracle.

Now that we have written the *findOracle* function we can look at the *updateOracle* function.

.. code:: haskell

    updateOracle :: forall w s. HasBlockchainActions s => Oracle -> Integer -> Contract w s Text ()
    updateOracle oracle x = do
        m <- findOracle oracle
        let c = Constraints.mustPayToTheScript x $ assetClassValue (oracleAsset oracle) 1
        case m of
            Nothing -> do
                ledgerTx <- submitTxConstraints (oracleInst oracle) c
                awaitTxConfirmed $ txId ledgerTx
                logInfo @String $ "set initial oracle value to " ++ show x
            Just (oref, o,  _) -> do
                let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>
                              Constraints.scriptInstanceLookups (oracleInst oracle) <>
                              Constraints.otherScript (oracleValidator oracle)
                    tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData Update)
                ledgerTx <- submitTxConstraintsWith @Oracling lookups tx
                awaitTxConfirmed $ txId ledgerTx
                logInfo @String $ "updated oracle value to " ++ show x
                
                
After the *findOracle* line there is a helper function definition, as we will need this constraint twice.

.. code:: haskell

    let c = Constraints.mustPayToTheScript x $ assetClassValue (oracleAsset oracle) 1

After looking for the oracle, there are wo possibilities - either we found it or we did not.

If we didn't find it, then we have started the oracle but we have not yet provided an initial value. This is the first case. And in this case, all we have to 
do is to submit a transaction that produces the first value for the oracle.

.. code:: haskell

    ledgerTx <- submitTxConstraints (oracleInst oracle) c
    awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "set initial oracle value to " ++ show x
    
Here is the first usage of the *c* helper function. It provides the constraint *mustPayToTheScript* which ensures that the transaction will have an output 
that pays to a script address. As arguments it takes the datum *x* and the NFT. The script that it must pay to is always the script that is in focus - here it is
the first parameter to *submitTxConstraints* - *(oracleInst oracle)*.

We then wait for confirmation and write a log message. And this is all we need to do for this case.

In the other case, where we already have a value, we need to reference the UTxO parts, but we don't care about the current datum, as we are going to update it anyway.

.. code:: haskell

    Just (oref, o,  _) -> do

Now it gets a bit more complicated, because now we need two conditions.

The first constraint is the same as in the other case - the constraint referenced by the helper function *c*. But there is now an extra constraint that we must also 
consume the existing UTxO.

.. code:: haskell

    tx = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData Update)

The *mustSpendScriptOutput* function is basically the opposite of *mustPayToTheScript*. It creates an input to this script address. As parameters it takes the reference 
to the UTxO we want to consume, and it takes a *Redeemer*. In this case the *Redeemer* is *Update* and it is converted to the Plutus *Data* type.

In order for this to work we need to provide some lookups.

In order to find the output *oref* that it wants to spend, we must use the *unspentOutputs* lookup, and in this case, we just provide the lookup with one UTxO.

.. code:: haskell

    Constraints.unspentOutputs (Map.singleton oref o)

Then we must provide the script instances. We need to do this twice, once for the input side, and once for the output side. For this, we provide the oracle instance and 
the oracle validator.

.. code:: haskell

    Constraints.scriptInstanceLookups (oracleInst oracle) <>
    Constraints.otherScript (oracleValidator oracle)

We didn't need to provide the *scriptInstanceLookups* lookup in the first case, as we were able to pass *oracleInst oracle* to the *submitTxConstraints* function. However,
with the *submitTxConstraintsWith* function, we don't have that option.

When submitting the transaction, we need to give the compiler a little nudge to let it know the script we are talking about - so that it knows, for example, what The Script is in *mustPayToTheScript*. For 
this we reference the *Oracling* type.

.. code:: haskell

    ledgerTx <- submitTxConstraintsWith @Oracling lookups tx

Hopefully now we have a valid transaction that gets submitted, and then we wait for it to be confirmed, and write some logging information.

.. code:: haskell

    awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "updated oracle value to " ++ show x
    
Remember, we talked about fee collecting earlier. This will happen automatically. The function *submitTxConstraintsWith* will send the fees to our own wallet. It does 
this because there is an imbalance between the value attached to the input, which includes the fees and the NFT, and the value we have said must be paid to the script,
which is just the NFT.

This process will also automatically create an extra input from our own input to pay the transaction fees for executing the transaction.

Lastly, we provide a function that combines these two operations, *startOracle* and *updateOracle* into one contract. This will make it possible to use in the playground 
and the *EmulatorTrace* monad, as well as in the PAB.

.. code:: haskell

    type OracleSchema = BlockchainActions .\/ Endpoint "update" Integer

    runOracle :: OracleParams -> Contract (Last Oracle) OracleSchema Text ()
    runOracle op = do
        oracle <- startOracle op
        tell $ Last $ Just oracle
        go oracle
      where
        go :: Oracle -> Contract (Last Oracle) OracleSchema Text a
        go oracle = do
            x <- endpoint @"update"
            updateOracle oracle x
            go oracle
    
The function *runOracle* first starts the oracle. Then, for reasons that will become clear later, we use *tell* to write the oracle parameter. We need to be able to
communicate the parameter value of the oracle to the outside world, so that people can use the oracle. We will not know until runtime the currency symbol that will be used
for the NFT, so we don't know the value of the oracle parameter yet.

Remember that *tell* expects a *Monoid* type. The typical example is a list of strings that get concatenated to one list of all log messages.

But it doesn't have to be lists. In *Data.Monoid* we have this *Last* Monoid.

.. code:: haskell

    Prelude Week06.Oracle.Core> import Data.Monoid (Last (..))
    Prelude Data.Monoid Week06.Oracle.Core> :i Last
    type Last :: * -> *
    newtype Last a = Last {getLast :: Maybe a}
          -- Defined in ‘Data.Monoid’
    instance Applicative Last -- Defined in ‘Data.Monoid’
    instance Eq a => Eq (Last a) -- Defined in ‘Data.Monoid’
    instance Functor Last -- Defined in ‘Data.Monoid’
    instance Monad Last -- Defined in ‘Data.Monoid’
    instance Monoid (Last a) -- Defined in ‘Data.Monoid’
    instance Ord a => Ord (Last a) -- Defined in ‘Data.Monoid’
    instance Semigroup (Last a) -- Defined in ‘Data.Monoid’
    instance Show a => Show (Last a) -- Defined in ‘Data.Monoid’
    instance Read a => Read (Last a) -- Defined in ‘Data.Monoid’
    instance Traversable Last -- Defined in ‘Data.Traversable’
    instance Foldable Last -- Defined in ‘Data.Foldable’

We see that it is just a *newtype* wrapper around *Maybe*. The point is to provide a specific *Monoid* instance. The idea, as the name suggests, is that it is
a monoid operation that always remembers the last Just value. For example:

.. code:: haskell

    Prelude Data.Monoid Week06.Oracle.Core> Last (Just 'x') <> Last (Just 'y')
    Last {getLast = Just 'y'}
    
However, if the second *Last* is a nothing, it will return the first one.

.. code:: haskell

    Prelude Data.Monoid Week06.Oracle.Core> Last (Just 'x') <> Last Nothing
    Last {getLast = Just 'x'}

If both are *Nothing*, it will be *Nothing*.

*Last* is very useful because it allows us to keep the current state. The value of the log will basically be the last *Just* we told.

In this contract we will only do that once. In the beginning it will be *Last Nothing*. Then we mint the NFT, and then, when we get the oracle value in 
*runOracle*, and then *tell* it, it will always have that value. If other contracts from the outside query the state, they will always get the *Just oracle*, so 
they will be able to discover the value of the oracle.

So, next in *runOracle*, we call the helper function *go*. What this does is to block at the update endpoint. As soon as someone provides an *Integer* as the new value,
it will call the *updateOracle* function with the new value, and then just loop to go again.

In summary, *runOracle* starts the oracle, *tell*\s the oracle, then loops to allow others to update the oracle.

And that concludes the code for the oracle itself. What is now missing is an example, a contract that actually uses the oracle - a swap contract. And also using the
Plutus Application Backend to run this code in the real world or, in our case, in a simulated blockchain.

Swap Validation
---------------
    
Our example swap contract can be found in

.. code:: haskell

    module Week06.Oracle.swap

The purpose of this contract is for someone to be able to deposit ADA and exchange it for a token, in our case a token that we will call USDT for US Dollar Token.

The idea is that the price, the amount of USDT that will be required to be paid for the ADA, will be determined by the value of the oracle. Remember that we are using
an *Integer* to reflect the exchange rate, with a value of one million being equal to one USDT.

We'll start with a helper function called *price*, which, given a number of lovelace and the exchange rate, returns the USDT price.

.. code:: haskell

    price :: Integer -> Integer -> Integer
    price lovelace exchangeRate = (lovelace * exchangeRate) `divide` 1000000
    
The next helper function, *lovelaces*, combines to functions from the Plutus libraries to extract a number of lovelace from a *Value* type.

.. code:: haskell

    lovelaces :: Value -> Integer
    lovelaces = Ada.getLovelace . Ada.fromValue    

Now we will write *mkSwapValidator*. This is a parameterized validator with two parameters. 

The first parameter is the oracle that we are using. To use this, we import the oracle module.

.. code:: haskell

    import Week06.Oracle.core

The second parameter is the address of the oracle. Normally, given the oracle, we would be able to compute the address from it. In the core module we saw a function
*oracleAddress* which does this for us. But this is a function that we can't use in the validator, because it can't be compiled to Plutus script. So, here, we explicitly 
hand the address to the validator.

For the datum, we use the public key hash of the seller. We don't use a redeemer, so we give it a type of Unit.

We recall from the diagram, the swap transaction should have three inputs and three outputs.

.. figure:: img/week06__00006.png

.. list-table:: Swap Transaction Inputs and Outputs
   :widths: 75 75
   :header-rows: 1

   * - Inputs
     - Outputs
   * - The oracle, to check the current exchange rate
     - The oracle, which we don't need to look at in the swap validation
   * - The swap UTxO that holds the lovelace
     - The tokens for the seller
   * - The source of the buyer's funds
     - The lovelace for the buyer
 
Note that we don't need to worry about the oracle as an output. The oracle validator takes care of ensuring that the value is not changed and that the fees are added.

We also want to support the second use case, the case where the seller can retrieve the ADA tokens in the case that they no longer want to do the swap. If we don't
support this case, the ADA could be locked there forever, if nobody ever decides to make the swap.

This second case is the condition we check in the validator. If the seller themselves signs the transaction, there are no further constraints - we don't need to check 
the oracle or anything else - the seller can just get back their lovelace.

.. code:: haskell

    mkSwapValidator :: Oracle -> Address -> PubKeyHash -> () -> ScriptContext -> Bool
    mkSwapValidator oracle addr pkh () ctx =
        txSignedBy info pkh ||
    ...            

The more interesting case is the second one, where we check two conditions. 

Firstly, there must be two inputs - the oracle and the swap UTxO. All additional inputs (the buyer's funds) must be public key inputs. This is because we don't want to worry about interference with other smart contracts.

Secondly, we want to check that the seller gets paid.

.. code:: haskell

    (traceIfFalse "expected exactly two script inputs" hasTwoScriptInputs &&
     traceIfFalse "price not paid"                     sellerPaid)    

Now, we have our helper function definitions.

First, the usual.

.. code:: haskell

    info :: TxInfo
    info = scriptContextTxInfo ctx
    
Then, we have *oracleInput* to get the UTxO from the oracle.

.. code:: haskell

    oracleInput :: TxOut
    oracleInput =
      let
        ins = [ o
              | i <- txInfoInputs info
              , let o = txInInfoResolved i
              , txOutAddress o == addr
              ]
      in
        case ins of
            [o] -> o
            _   -> traceError "expected exactly one oracle input"
            
We do this by getting a list of all the inputs. For this we use list comprehension, which allows us to draw from other lists using a filter. In this case we draw
from the list from *txInfoInputs info*, which is a list of *TxInfo*. We use the *txInInfoResolved* function to look at each element as a *TxOut* type, which we then compare with the
*addr* parameter. The resulting list will either by empty, or will have the *TxOut* that matches the oracle UTxO.

We then check that there is exactly one element in the resulting list, and, if there is, we return it. We don't return the list, just the *TxOut*.

This has now given us the oracle output that we are consuming as an input.

Now, we want to check the actual exchange rate. For that, we use the *oracleValue* function that we defined in the core module. Here again, it may succeed, or it may 
fail. If it succeeds we return the value.

.. code:: haskell

    oracleValue' = case oracleValue oracleInput (`findDatum` info) of
        Nothing -> traceError "oracle value not found"
        Just x  -> x    

We do not need to check whether the oracle contains the NFT. Due to the way validation works for the oracle, we know that it is present.

Now, let's look at the *hasTwoScriptInputs* helper function.

.. code:: haskell

    hasTwoScriptInputs :: Bool
    hasTwoScriptInputs =
      let
        xs = filter (isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
      in
        length xs == 2

First, we filter, using the composite function

.. code:: haskell

    (isJust . toValidatorHash . txOutAddress . txInInfoResolved)

Reading right to left, we get the UTxO from the input, then we get the address for this UTxO, then we get the validator hash for that address. Then, finally, we check
if it is a script output, by seeing if it is a *Just*. If it is a *Nothing*, then this would show that it is a public key, not a script address.

We then use this composite function as a filter against the list of *TxInInfo*\s. And then we check that the length of the resulting list is exactly two.

Going back to our validation conditions, we now have to deal with checking that the seller is getting paid. So let's write the *sellerPaid* helper function that
we referenced.

For this we will use another helper function to determine the required price.

.. code:: haskell

    minPrice :: Integer
    minPrice =
      let
        lovelaceIn = case findOwnInput ctx of
            Nothing -> traceError "own input not found"
            Just i  -> lovelaces $ txOutValue $ txInInfoResolved i
      in
        price lovelaceIn oracleValue'    

First we check that we have an input, and if so, we extract the number of lovelaces and assign that number to *lovelaceIn*. Then, we use the *price* helper function
to determine the price in USD tokens.

Now, we can define the *sellerPaid* helper function.

.. code:: haskell

    sellerPaid :: Bool
    sellerPaid =
      let
        pricePaid :: Integer
        pricePaid =  assetClassValueOf (valuePaidTo info pkh) (oAsset oracle)
      in
        pricePaid >= minPrice

The function *valuePaidTo* is from the Plutus libraries. Given *info* and a public key hash, it will add up all the values of all the public key outputs that go
to this address. We then use the *assetClassValueOf* function to check the component of the value that is in USD token, and the check that we have at least as many
as we require.

That's the end of the main part of the code for the swap validator. We just have our normal boiler plate to write.

.. code:: haskell

    data Swapping
    instance Scripts.ScriptType Swapping where
        type instance DatumType Swapping = PubKeyHash
        type instance RedeemerType Swapping = ()
    
    swapInst :: Oracle -> Scripts.ScriptInstance Swapping
    swapInst oracle = Scripts.validator @Swapping
        ($$(PlutusTx.compile [|| mkSwapValidator ||])
            `PlutusTx.applyCode` PlutusTx.liftCode oracle
            `PlutusTx.applyCode` PlutusTx.liftCode (oracleAddress oracle))
        $$(PlutusTx.compile [|| wrap ||])
      where
        wrap = Scripts.wrapValidator @PubKeyHash @()
    
    swapValidator :: Oracle -> Validator
    swapValidator = Scripts.validatorScript . swapInst
    
    swapAddress :: Oracle -> Ledger.Address
    swapAddress = scriptAddress . swapValidator
    
Note that in the *swapInst* function, where we use template haskell to generate the Plutus validator from the *mkSwapValidator* function, we do not need to pass in the 
oracle address as a parameter. This is because we will compute this inside the function. Remember that we can't use the *oracleAddress* function inside the Plutus validator.

Now to define some contracts.

offerSwap
~~~~~~~~~

First *offerSwap*. This is for a seller who wants to offer a certain number of lovelace for exchange.

.. code:: haskell

    offerSwap :: forall w s. HasBlockchainActions s => Oracle -> Integer -> Contract w s Text ()
    offerSwap oracle amt = do
        pkh <- pubKeyHash <$> Contract.ownPubKey
        let tx = Constraints.mustPayToTheScript pkh $ Ada.lovelaceValueOf amt
        ledgerTx <- submitTxConstraints (swapInst oracle) tx
        awaitTxConfirmed $ txId ledgerTx
        logInfo @String $ "offered " ++ show amt ++ " lovelace for swap"    

findSwaps
~~~~~~~~~

Next, a helper function that will find all swaps that satisfy a given predicate. It takes an oracle plus a predicate based on public key hashes, and returns a list 
of triples of the UTxOs that satisfy the predicate.

.. code:: haskell

    findSwaps :: HasBlockchainActions s => Oracle -> (PubKeyHash -> Bool) -> Contract w s Text [(TxOutRef, TxOutTx, PubKeyHash)]
    findSwaps oracle p = do
        utxos <- utxoAt $ swapAddress oracle
        return $ mapMaybe g $ Map.toList utxos
      where
        f :: TxOutTx -> Maybe PubKeyHash
        f o = do
            dh        <- txOutDatumHash $ txOutTxOut o
            (Datum d) <- Map.lookup dh $ txData $ txOutTxTx o
            PlutusTx.fromData d
    
        g :: (TxOutRef, TxOutTx) -> Maybe (TxOutRef, TxOutTx, PubKeyHash)
        g (oref, o) = do
            pkh <- f o
            guard $ p pkh
            return (oref, o, pkh)
            
First, we get a list of all the UTxOs sitting at the swap contract address. We then apply *mapMaybe* to this list.

.. code:: haskell

    mapMaybe :: (a -> Maybe b) -> [a] -> [b]
    
This function will apply the *(a -> Maybe b)* function to each element in a list of *a*\s and creates a list of *Maybe b*\s, which could contain a mixture of *Just*\s
and *Nothing*\s. It then throws away the *Nothing*\s and returns the values contained in the *Just*\s.

To clarify this, imagine we have a function that returns as *Just* for even numbers and a *Nothing* for odd numbers.

.. code:: haskell

    f (n :: Int) = if even n then Just (div n 2) else Nothing

We can use this as the first parameter to map Maybe

.. code:: haskell

    Prelude Week06.Oracle.Core> import Data.Maybe
    Prelude Data.Maybe Week06.Oracle.Core> mapMaybe f [2, 4, 10, 11, 13, 100]
    [1,2,5,50]
    
We use the *mapMaybe* and the function *g* to filter the list of UTxOs.

.. code:: haskell

    g :: (TxOutRef, TxOutTx) -> Maybe (TxOutRef, TxOutTx, PubKeyHash)
    g (oref, o) = do
        pkh <- f o
        guard $ p pkh
        return (oref, o, pkh)
        
This function takes a key value pair representing the UTxO and returns a *Maybe* triple containing the items from the pair alongside a *PubKeyHash*.     

Function *g* is inside the *Maybe* monad and makes use of function *f*, which is also inside the *Maybe* monad. Function *f* gets the public key hash from a UTxO,
if it exists. After this, function *g* uses the *guard* function with the predicate function *p* that we passed in as an argument.

The *guard* function is available in some monads, and the *Maybe* monad is one of them. It takes a boolean as a parameter, and, if the boolean is false, the computation 
fails. In this case, failure means returning *Nothing*. If it is true, it just continues. In this case, that means returning the *Just* of the triple containing the 
public key hash.

We will see how we use the *findSwaps* function in a moment.

retrieveSwaps
~~~~~~~~~~~~~

The *retrieveSwaps* contract is for the seller if they want to change their mind and get their Ada back.

Here is where we use the *findSwaps* function. We use it with *(== pkh)* as the predicate, meaning that we want only those UTxOs sitting at the swap address that
belong to the operator.

.. code:: haskell

    retrieveSwaps :: HasBlockchainActions s => Oracle -> Contract w s Text ()
    retrieveSwaps oracle = do
        pkh <- pubKeyHash <$> ownPubKey
        xs <- findSwaps oracle (== pkh)
        case xs of
            [] -> logInfo @String "no swaps found"
            _  -> do
                let lookups = Constraints.unspentOutputs (Map.fromList [(oref, o) | (oref, o, _) <- xs]) <>
                              Constraints.otherScript (swapValidator oracle)
                    tx      = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | (oref, _, _) <- xs]
                ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
                awaitTxConfirmed $ txId ledgerTx
                logInfo @String $ "retrieved " ++ show (length xs) ++ " swap(s)"
                
If there are none, then there is nothing to do. If there are some, we construct a transaction that retrieves all of them.

To do that, we create a list of *mustSpendScriptOutput* constraints.

.. code:: haskell

    tx = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | (oref, _, _) <- xs]

The line looks intimidating, but it is just extracting a list of *oref*\s from the *xs* list and using it to construct a constraint for each of them, using *Unit* as
the *Redeemer* type. The function *mconcat* applies the *Semigroup* operator *<>* throughout the list in order to combine them.

As lookups, we must provide all the UTxOs and the swap validator.

We have the list of UTxOs in *xs* and we use list comprehension to turn this list into a list of pairs, and we then use *Map.fromList* to turn those pairs into a 
map, to which we then apply the *unspentOutputs* constraint.

useSwaps
~~~~~~~~

And now the most interesting one, *useSwaps*. This is where we actually use the oracle.

.. code:: haskell

    useSwap :: forall w s. HasBlockchainActions s => Oracle -> Contract w s Text ()
    useSwap oracle = do
        funds <- ownFunds
        let amt = assetClassValueOf funds $ oAsset oracle
        logInfo @String $ "available assets: " ++ show amt
    
        m <- findOracle oracle
        case m of
            Nothing           -> logInfo @String "oracle not found"
            Just (oref, o, x) -> do
                logInfo @String $ "found oracle, exchange rate " ++ show x
                pkh   <- pubKeyHash <$> Contract.ownPubKey
                swaps <- findSwaps oracle (/= pkh)
                case find (f amt x) swaps of
                    Nothing                -> logInfo @String "no suitable swap found"
                    Just (oref', o', pkh') -> do
                        let v       = txOutValue (txOutTxOut o) <> lovelaceValueOf (oFee oracle)
                            p       = assetClassValue (oAsset oracle) $ price (lovelaces $ txOutValue $ txOutTxOut o') x
                            lookups = Constraints.otherScript (swapValidator oracle)                     <>
                                      Constraints.otherScript (oracleValidator oracle)                   <>
                                      Constraints.unspentOutputs (Map.fromList [(oref, o), (oref', o')])
                            tx      = Constraints.mustSpendScriptOutput oref  (Redeemer $ PlutusTx.toData Use) <>
                                      Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toData ())  <>
                                      Constraints.mustPayToOtherScript
                                        (validatorHash $ oracleValidator oracle)
                                        (Datum $ PlutusTx.toData x)
                                        v                                                                      <>
                                      Constraints.mustPayToPubKey pkh' p
                        ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
                        awaitTxConfirmed $ txId ledgerTx
                        logInfo @String $ "made swap with price " ++ show (Value.flattenValue p)
      where
        getPrice :: Integer -> TxOutTx -> Integer
        getPrice x o = price (lovelaces $ txOutValue $ txOutTxOut o) x
    
        f :: Integer -> Integer -> (TxOutRef, TxOutTx, PubKeyHash) -> Bool
        f amt x (_, o, _) = getPrice x o <= amt
        
First, we use the *ownFunds* function. This is defined in a separate module that we will get to in a bit. All it does is to add up all the money in our own
wallet and returns a *Value*. We then find out how many USD Tokens we have.

.. code:: haskell

    funds <- ownFunds
    let amt = assetClassValueOf funds $ oAsset oracle
    logInfo @String $ "available assets: " ++ show amt
    
The *findOracle* function is defined in the Oracle.Core module from earlier. You will recall that it finds us the oracle UTxO that contains the oracle value.

.. code:: haskell

    m <- findOracle oracle

If we don't find the oracle, we will just log a message to that effect.

.. code:: haskell

    case m of
        Nothing           -> logInfo @String "oracle not found"
        
If we do find it, we will log a message with the current exchange rate.

.. code:: haskell

    Just (oref, o, x) -> do
        logInfo @String $ "found oracle, exchange rate " ++ show x    

Next, we check our own public key and check for all available swaps where we are *not* the owner.

.. code:: haskell

    pkh   <- pubKeyHash <$> Contract.ownPubKey
    swaps <- findSwaps oracle (/= pkh)    

Then, we use a function *find* which is from the Haskell prelude, in module *Data.List*. The *find* function takes a predicate and a list and *Maybe* returns one 
element of that list that satisfies the predicate.

The function used in the predicate is defined as a helper function.

.. code:: haskell

    where
        getPrice :: Integer -> TxOutTx -> Integer
        getPrice x o = price (lovelaces $ txOutValue $ txOutTxOut o) x    

        f :: Integer -> Integer -> (TxOutRef, TxOutTx, PubKeyHash) -> Bool
        f amt x (_, o, _) = getPrice x o <= amt    

We give it an amount, the current exchange rate, and a UTxO triple. The function determines if there is a swap that is cheaper to or equal to the amount parameter.

Now, we have searched for a swap that we can afford. If we don't find one, we log a message saying so.

.. code:: haskell

    case find (f amt x) swaps of
        Nothing                -> logInfo @String "no suitable swap found"

If we *do* find one, we just take the first one. This isn't very realistic, of course. In a real-world example we would probably specify the exact amount we want to swap. Here,
we are just keeping it simple as we are focussed on oracles rather than swapping.



            























        










