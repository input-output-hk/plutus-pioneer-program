Week 08 - Property Based Testing
================================

.. note::
    This is a written version of `Lecture
    #8 <https://youtu.be/JMRwkMgaBOg>`__.

    In this lecture we cover another state machine example, automatic testing using emulator traces, optics, and property-based testing.

    This week we were using Plutus commit ae35c4b8fe66dd626679bd2951bd72190e09a123, the same commit as we used in the last lecture.

Token Sale
----------

In the last lecture we looked at state machines, and saw how they often allow us to write much less code to express the logic of a smart contract, partly because  
there is a lot of sharing between on-chain and off-chain code and partly because a lot of boilerplate is encapsulated in the state machine machinery.

In this lecture we will see another example using a state machine, because the concept is very important. We will also take a look at testing. 
First we will look at the code, then we will explore various ways to go about testing.

The example we will use is a contract that allows somebody to sell tokens. The idea is that someone call lock some tokens in a contract, set a price, and then 
other people can buy them.

To begin, the seller starts with an NFT. It can be an arbitrary NFT and it will just be used, as before, to identify the correct UTxO that contains the contract state.

The first step is to lock the NFT at the script address of the smart contract that we are about to write. We'll call that contract *TS* for Token Sale. As a datum, we will 
use a simple integer, which will represent the price of the token we are selling, and this will start off as zero.

.. figure:: img/week08__00001.png

There will be several operations that the seller can do. One of those will be setting the price to a different value. In order to do that the seller will submit
a transaction which has the current UTxO as input and the updated UTxO as output, where the datum has been changed to a different price per token.

.. figure:: img/week08__00002.png

Another thing that the seller can do is to lock some tokens in the contract. In order to do that they have to create another transaction which has as input the UTxO of
the contract and a UTxO containing some tokens and, as output, the updated UTxO at the contract address which now contains the provided tokens.

.. figure:: img/week08__00003.png

In this example, the seller provides five tokens to the contract.

In order to buy tokens, there needs to be a transaction created by the buyer. This transaction has as input the UTxO sitting at the TS script address, 
and the buying price in Ada.

So, if a buyer wants to buy two tokens, they will create a transaction that has, as input, 12 Ada, and the UTxO at the script address. Then, two outputs. One
the updated contract state where now the tokens are taken out and the Ada has been added, and one output going to the buyer with the tokens that they have just
bought.

.. figure:: img/week08__00004.png

Finally, there must be a way for the seller to retrieve tokens and Ada. In this example if, after the sale, the seller wants to retrieve all the Ada and one token, they 
would create a transaction that, again, has the script UTxO as input, and, as output, the updated script UTxO with the reduced balances, and one to themselves with the 
retrieved funds.

The diagram just shows one scenario, but these operations can be performed in any order - tokens can be added, the price can be changed, tokens can be bought, and so on,
in an arbitrary order.

On-chain code 
~~~~~~~~~~~~~

This week's first example is implemented in

.. code:: haskell

    module Week08.TokenSale

Let's first look at the type that we will use as the parameter that we will use for the contract.    

.. code:: haskell

    data TokenSale = TokenSale
        { tsSeller :: !PubKeyHash
        , tsToken  :: !AssetClass
        , tsNFT    :: !AssetClass
        } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)    

This has three fields - the seller's public key has, the token being sold, and the NFT used to identify the UTxO.

For the redeemer, we provide exactly the operations we saw in the diagram

.. code:: haskell

    data TSRedeemer =
          SetPrice Integer         -- the price
        | AddTokens Integer        -- the number of tokens to add
        | BuyTokens Integer        -- the number of tokens to buy
        | Withdraw Integer Integer -- first argument is the number of tokens, the second is the number of lovelace
        deriving (Show, Prelude.Eq)    

Again we have the helper function that we have used in previous examples

.. code:: haskell

    lovelaces :: Value -> Integer
    lovelaces = Ada.getLovelace . Ada.fromValue
    
Now, we get to the *transition* function of the state machine. We see the *TokenSale* parameter which holds the state machines configuration values, the *State* 
object with an *Integer* value to represent the price of the token, then the redeemer *TsRedeemer*. Again, we return a *Maybe*, which will be *Nothing* if the 
corresponding transition is illegal, or, if it is legal, a *Just* containing constraints and the new state.

.. code:: haskell

    transition :: TokenSale -> State Integer -> TSRedeemer -> Maybe (TxConstraints Void Void, State Integer)
    transition ts s r = case (stateValue s, stateData s, r) of

If the *SetPrice* redeemer is provided, then we only consider it to be legal if the price is not negative. We then return a *Just* with the constraint that the 
transaction must be signed by the token seller, and with the new state. The new state will be the new price *p*, and the *Value* in the contract 
remains the same, except for one thing. 

It is a little unfortunate, but there is a discrepancy between the *v* on the left and the *v* on the right. On the left it does not contain the NFT, but 
on the right it does not. So, even though we want to say that we don't want the value changed, in fact we have to remove the NFT, because the Plutus libraries will
add it again. This is perhaps not an ideal design, but that is how it currently is.

.. code:: haskell

        (v, _, SetPrice p) | p >= 0 -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                            , State p $
                                              v <>
                                              nft (negate 1)
                                            )

We use a helper function to reference the NFT.

.. code:: haskell

  nft :: Integer -> Value
  nft = assetClassValue (tsNFT ts)  
                                            
When adding tokens, we could check that the seller has signed the transaction, but this contract would be provided by the seller, and the seller doesn't mind if someone 
wants to give them a free gift! Therefore, once we have the *AddTokens* redeemer and *n* is greater than zero, we are happy to return the 
new state without constraints.

The state that we return is untouched, except for the unfortunate trick we need to do with the NFT, and the addition of the new tokens.

.. code:: haskell

        (v, p, AddTokens n) | n > 0 -> Just ( mempty
                                            , State p $
                                              v                                       <>
                                              nft (negate 1)                          <>
                                              assetClassValue (tsToken ts) n
                                            )

For the *BuyTokens* redeemer, again we check the number of tokens is positive, and again we don't need any constraints, because anybody can buy tokens.

For the new state, we don't touch the price. We again correct for the NFT. Then we subtract the tokens that were bought, and we add the lovelace that were paid for them.

.. code:: haskell
                                                                
        (v, p, BuyTokens n) | n > 0 -> Just ( mempty
                                            , State p $
                                              v                                       <>
                                              nft (negate 1)                          <>
                                              assetClassValue (tsToken ts) (negate n) <>
                                              lovelaceValueOf (n * p)
                                            )

Finally, for *WithDraw*, we insist that the token amount and the lovelace amount are both nonnegative. This time we again add a constraint that the seller must sign 
the transaction. We modify the state in a similar way to the way we did for the *BuyTokens* redeemer, but this time we adjust the token and lovelace amounts according 
to how much has been withdrawn.

.. code:: haskell
                                                           
        (v, p, Withdraw n l) | n >= 0 && l >= 0 -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                        , State p $
                                                          v                                       <>
                                                          nft (negate 1)                          <>
                                                          assetClassValue (tsToken ts) (negate n) <>
                                                          lovelaceValueOf (negate l)
                                                        )

All other state transitions are illegal.

.. code:: haskell
  
        _ -> Nothing

In this example we are able to construct our state machine more simply that we could in the previous lecture. This is because, in the previous lecture we had one 
condition that could not be expressed in the regular constraints.        

In these situations, there is a helper function called *mkStateMachine* that takes three arguments. The first one is the state token, the second is the transition 
function. The last one is to indicate which states are final. In this case, there is no final state. Once this token sale has been setup, it will always be there.

.. code:: haskell

  tsStateMachine :: TokenSale -> StateMachine Integer TSRedeemer
  tsStateMachine ts = mkStateMachine (Just $ tsNFT ts) (transition ts) (const False)

We can now use the usual boilerplate to turn it into a Plutus smart contract.

.. code:: haskell

  type TS = StateMachine Integer TSRedeemer

  tsInst :: TokenSale -> Scripts.ScriptInstance TS
  tsInst ts = Scripts.validator @TS
      ($$(PlutusTx.compile [|| mkTSValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode ts)
      $$(PlutusTx.compile [|| wrap ||])
    where
      wrap = Scripts.wrapValidator @Integer @TSRedeemer
  
  tsValidator :: TokenSale -> Validator
  tsValidator = Scripts.validatorScript . tsInst
  
  tsAddress :: TokenSale -> Ledger.Address
  tsAddress = scriptAddress . tsValidator
  
  tsClient :: TokenSale -> StateMachineClient Integer TSRedeemer
  tsClient ts = mkStateMachineClient $ StateMachineInstance (tsStateMachine ts) (tsInst ts)
  
There are two helper functions to convert specialised error types to *Text*.

.. code:: haskell

  mapErrorC :: Contract w s C.CurrencyError a -> Contract w s Text a
  mapErrorC = mapError $ pack . show
  
  mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
  mapErrorSM = mapError $ pack . show
  
Off-chain code 
~~~~~~~~~~~~~~
  
For the off-chain code, we start by defining a constant for the token name of the NFT.

.. code:: haskell

  nftName :: TokenName
  nftName = "NFT"

The first contract we define is to start the token sale. This contract is designed to be invoked by the seller.

This first argument is a *Maybe CurrencySymbol*. The idea here is that if you pass in *Nothing*, the contract will mint a new NFT. Alternatively, you can provide a 
*Just CurrencySymbol* if the token already exists. We have done it this way mainly to make testing easier.

The *AssetClass* argument is the token the seller wants to trade.

For the return type, we are using the writer monad type with the *Last* type. The ideas is that once the token sale has been setup, it will get written here so that other 
contracts are able to discover it. In addition, we return the created token sale.

To begin, we lookup the seller's public key hash. We then need to get hold of the NFT. So, we determine if we need to mint the NFT, and, if we do, we mint it, otherwise we just use the one that was 
passed into the function.

.. code:: haskell

  startTS :: HasBlockchainActions s => Maybe CurrencySymbol -> AssetClass -> Contract (Last TokenSale) s Text TokenSale
  startTS mcs token = do
  
      pkh <- pubKeyHash <$> Contract.ownPubKey
      cs  <- case mcs of
          Nothing  -> C.currencySymbol <$> mapErrorC (C.forgeContract pkh [(nftName, 1)])
          Just cs' -> return cs'

And now we can define the *TokenSale* and create the state machine client.

.. code:: haskell
  
      let ts = TokenSale
              { tsSeller = pkh
              , tsToken  = token
              , tsNFT    = AssetClass (cs, nftName)
              }
          client = tsClient ts

We then use the *runInitialise* function that we discussed in the last lecture, using the client, an initial price of zero, and no initial funds, except for the NFT 
which will be automatically added.

We write the *ts* into the log, then log a message, and return the *ts*.

.. code:: haskell

      void $ mapErrorSM $ runInitialise client 0 mempty
      tell $ Last $ Just ts
      logInfo $ "started token sale " ++ show ts
      return ts
      
The functions for all the other operations are extremely short. This example is ideal for the state machine approach.

They are all very similar. They all invoke *runStep* and then invoke the correct transition from the state machine.

For example, for *setPrice*, we need the *TokenSale* argument to identify the correct contract and the new value of the price. Then we use *runStep* using the client and 
*SetPrice* as the redeemer. We wrap that using *mapErrorSM* to convert to *Text* error messages, and we ignore the result.

.. code:: haskell

  setPrice :: HasBlockchainActions s => TokenSale -> Integer -> Contract w s Text ()
  setPrice ts p = void $ mapErrorSM $ runStep (tsClient ts) $ SetPrice p

The remaining three follow the same pattern.

.. code:: haskell

  addTokens :: HasBlockchainActions s => TokenSale -> Integer -> Contract w s Text ()
  addTokens ts n = void (mapErrorSM $ runStep (tsClient ts) $ AddTokens n)
  
  buyTokens :: HasBlockchainActions s => TokenSale -> Integer -> Contract w s Text ()
  buyTokens ts n = void $ mapErrorSM $ runStep (tsClient ts) $ BuyTokens n
  
  withdraw :: HasBlockchainActions s => TokenSale -> Integer -> Integer -> Contract w s Text ()
  withdraw ts n l = void $ mapErrorSM $ runStep (tsClient ts) $ Withdraw n l

Now we define three schemas.

One for the seller which just has one endpoint which takes the *CurrencySymbol* and the *TokenName* of the asset to be traded.

.. code:: haskell

  type TSStartSchema = BlockchainActions
      .\/ Endpoint "start"      (CurrencySymbol, TokenName)

For testing purposes, we create *TSStartSchema'* which additionally takes the *CurrencySymbol* of the NFT.

.. code:: haskell

  type TSStartSchema' = BlockchainActions
      .\/ Endpoint "start"      (CurrencySymbol, CurrencySymbol, TokenName)  
  
Lastly we have a *use* schema, with endpoints for the four operations - set price, add tokens, buy tokens and withdraw. 

.. code:: haskell

  type TSUseSchema = BlockchainActions
    .\/ Endpoint "set price"  Integer
    .\/ Endpoint "add tokens" Integer
    .\/ Endpoint "buy tokens" Integer
    .\/ Endpoint "withdraw"   (Integer, Integer)  

Now to implement the start endpoint. It simply calls *startTs'* and recurses. *startTs'* blocks until the parameters are provided and then calls *startTs* with 
*Nothing*, indicating that the NFT has to be minted. We wrap it in *handleError* and if there is an error, we simply log that error.

.. code:: haskell

  startEndpoint :: Contract (Last TokenSale) TSStartSchema Text ()
  startEndpoint = startTS' >> startEndpoint
    where
      startTS' = handleError logError $ endpoint @"start"  >>= void . startTS Nothing . AssetClass

The *startEndpoint'* function is very similar, but we add the NFT parameter, as per *TSStartSchema'*.

.. code:: haskell

  startEndpoint' :: Contract (Last TokenSale) TSStartSchema' Text ()
  startEndpoint' = startTS' >> startEndpoint'
    where
      startTS' = handleError logError $ endpoint @"start"  >>= \(cs1, cs2, tn) ->  void $ startTS (Just cs1) $ AssetClass (cs2, tn)
      
No surprises in the *use* endpoints. We give a choice between the four endpoints and just call the functions we defined earlier with the arguments fed in 
from the endpoint call, and with everything wrapped inside an error handler so that the contract won't crash in the event of an error.

.. code:: haskell

  useEndpoints :: TokenSale -> Contract () TSUseSchema Text ()
  useEndpoints ts = (setPrice' `select` addTokens' `select` buyTokens' `select` withdraw') >> useEndpoints ts
    where
      setPrice'  = handleError logError $ endpoint @"set price"  >>= setPrice ts
      addTokens' = handleError logError $ endpoint @"add tokens" >>= addTokens ts
      buyTokens' = handleError logError $ endpoint @"buy tokens" >>= buyTokens ts
      withdraw'  = handleError logError $ endpoint @"withdraw"   >>= uncurry (withdraw ts)

Testing
~~~~~~~

In order to try it out, let's run it in the emulator.

We define a *runMyTrace* function which uses *runEmulatorTraceIO'* with a custom emulator configuration and a *myTrace* function.

.. code:: haskell

  runMyTrace :: IO ()
  runMyTrace = runEmulatorTraceIO' def emCfg myTrace

Let's first look at the *emCfg* function. Recall that this is where we can give custom initial distributions to wallets. Here we give 1000 Ada and 1000 of a custom 
token to three wallets.

.. note::
  
  The ability to use underscores in large numbers such as 1000_000_000 is provided by a GHC extension *NumericUnderscores*

.. code:: haskell

  emCfg :: EmulatorConfig
  emCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet w, v) | w <- [1 .. 3]]
    where
      v :: Value
      v = Ada.lovelaceValueOf 1000_000_000 <> assetClassValue token 1000
      
  currency :: CurrencySymbol
  currency = "aa"
  
  name :: TokenName
  name = "A"
  
  token :: AssetClass
  token = AssetClass (currency, name)      

For the trace, first we activate Wallet 1 using the non-primed *startEndpoint* function which mints the NFT is minted automatically. Then, we call the start endpoint, giving it 
the symbol and name of the token we want to sell, and then wait for five slots, although two would be enough in this case.

.. code:: haskell

  myTrace :: EmulatorTrace ()
  myTrace = do
      h <- activateContractWallet (Wallet 1) startEndpoint  
      callEndpoint @"start" h (currency, name)
      void $ Emulator.waitNSlots 5
      Last m <- observableState h

We then read the state, which we wrote using *tell*, and check to see if it is valid. If it is not, we log an error. If it is, we proceed with the test.

.. code:: haskell

  case m of
    Nothing -> Extras.logError @String "error starting token sale"
    Just ts -> do  
        Extras.logInfo $ "started token sale " ++ show ts

We can now activate the endpoints for the three wallets. Recall that the *useEndpoints* function is parameterised by the *TokenSale* data, which is why we needed to 
get that value.

.. code:: haskell

    h1 <- activateContractWallet (Wallet 1) $ useEndpoints ts
    h2 <- activateContractWallet (Wallet 2) $ useEndpoints ts
    h3 <- activateContractWallet (Wallet 3) $ useEndpoints ts  

Wallet 1 sets the price to 1 Ada and we again wait for a generous amount of time.

.. code:: haskell

  callEndpoint @"set price" h1 1_000_000
  void $ Emulator.waitNSlots 5  

Wallet 1 adds 100 tokens.

.. code:: haskell

  callEndpoint @"add tokens" h1 100
  void $ Emulator.waitNSlots 5
  
Wallet 2 buys 20 tokens. So now the contract should contain 80 tokens and 20 Ada.

.. code:: haskell

  callEndpoint @"buy tokens" h2 20
  void $ Emulator.waitNSlots 5
  
Wallet 3 buys 5 tokens. Now there should be 75 tokens in the contract and 25 Ada.

.. code:: haskell

  callEndpoint @"buy tokens" h3 5
  void $ Emulator.waitNSlots 5

Finally, Wallet 1 calls the withdraw endpoint, taking out 40 tokens and 10 Ada. At this point, there should be 35 tokens and 10 Ada in the contract.

.. code:: haskell

  callEndpoint @"withdraw" h1 (40, 10_000_000)
  void $ Emulator.waitNSlots 5
  
Let's run this in the REPL.

.. code::

  cabal repl plutus-pioneer-program-week08-tests
  Ok, five modules loaded.
  Prelude Main> :l Spec.Trace
  Ok, one module loaded.
  Prelude Spec.Trace> runMyTrace

  
  
  


  
  