# Lecture 2 notes
Lecture notes based on the first ever official [Plutus-Pioneer program](https://github.com/input-output-hk/plutus-pioneer-program). This notes follow the [YouTube lecture 2](https://www.youtube.com/watch?v=E5KRk5y9KjQ/).

Before getting started you should clone the [Plutus repository](https://github.com/input-output-hk/plutus#prerequisites) into your local machine. To follow this guide first create a master directory. Inside this directory we will clone both the main Plutus and the Plutus-Pioneer repo.

    $ mkdir Cardano-King && cd Cardano-King
    $ git clone https://github.com/input-output-hk/plutus.git
    $ git clone https://github.com/input-output-hk/plutus-pioneer-program.git
    
    
## 0. Outline
1. [Get started with the cabal repl](<#secc:get-started>)
1. [Implement our first validator](<#secc:get-validator>)


## 0.1 Get started with the cabal repl

Since we are not installing cabal locally, bacause it is much more convenient to use it from nix-shell (this is included in [Nix](https://nixos.org/nix/)), we will navigate to the plutus-pioneer/week02 git-cloned repo inside a nix environment. However, nix must be started first from the plutus root repo.

    $ cd ./plutus/  
    $ nix-shell
    nix$ cd ../plutus-pioneer-program/code/week02
    nix$ cabal repl
    
Now we are isnide the cabal repl (takes a while the first time). Its time to test some varaibles defined in the plutus libraries that eventually we will get very familiar with

    repl$ import PlutusTx
    
Some examples seeing the type of Data and create bite-string on the fly (use literal strings to create byte strings), and then test it (`:t B ...`)
    
    repl$ :i Data
    repl$ :set -XOverloadedStrings  ## allows to include extremely large strings (literal-string to byte-strings)
    repl$ :t B "Haskellanians"
    


## 1. [Implement our first validator](<#secc:get-validator>)
Start a Haskell module "Week02.Gift" where the script imports all the necessary language extensions that plutus needs. (first 8 lines `{-#...#-}`) and other modules that we refer to (lines 12-28) from the `./week01/EnglishAuction.hs` script.

Go back to the repl and load Gift.hs. Then we will see what a Monoid in haskell `()`

    repl$ :l ./src/Week02/Gift.hs 
    repl$ :t ()
    
We can open Gift.hs in a text editor and see that lines 31-35 contain the implementation of Monoid referring to the [#secc:get-sstarted] section above.
    

## 2. Explore the first contract, the [`Gift.hs`](https://github.com/Igodlab/plutus-pioneer-program/blob/main/code/week02/src/Week02/Gift.hs) contract.

We will breakdown some of the sections of the code that give the functionality to the contract. In the `Gift.hs` contract

#### 2.1 start the contract void
    
    mkValidator :: Data -> Data -> Data -> ()
    mkValidator _ _ _ = ()
    
Which basically is a Haskell function (`mkValidator`), that latter on will be compiled into a Plutus function. It just creates a function that regardless of the inputs proceeds to the reading of the following lines of the code. 

#### 2.2 create a validator
The next step is to create a validator (`mkValidatorScript`), this one uses **template Haskell**  this basically compiles inline (allowed by the `{~#INLINABLE mkValidator#~}` pragma) everything after the *`$$` splice* (after double dollar sign `$$`) in this case it will invoke the compilation of `PlutusTx.compile` with its input`mkValidator` (this is whatever is inside the double bars, inside the square brakets).

    validator :: Validator
    validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])
    
#### 2.3 create a Plutus address
Next, we create a validator hash to then turn in into an actual address (not a public key address yet, this is a Plutus address)

    valHash :: Ledger.ValidatorHash
    valHash = Scripts.validatorHash validator
    
    scrAddress :: Ledger.Address
    scrAdress = ScriptAdress valHash

#### 2.4 contract functionalities    
In order to try this we need wallet code as follows below

    typeGiftSchema = 
        BlockchainAction
            .\/ Endpoint "give" Integer -- takes an integer as input
            .\/ Endpoint "grab" ()      -- takes no input, monoid () allows to keep running
            
    give :: (HasBlockchainActions s, AsContractError e) = Integer -> Contract w s e ()
    give amount = do
        let tx = mustPayToOtherScript valHash (Datum $ Constr 0 []) $ Ada.lovelaceValueOf amount
        ledgerTx <- submitTx tx
        void $ awaitTxConfirmed $ txId ledgerTx
        logInfo @String $ printf "made a gift of %d lovelace" amount

    grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => Contract w s e ()
    grab = do
        utxos <- utxoAt scrAddress
        let orefs   = fst <$> Map.toList utxos
            lookups = Constraints.unspentOutputs utxos      <>
                      Constraints.otherScript validator
            tx :: TxConstraints Void Void
            tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ I 17 | oref <- orefs]
        ledgerTx <- submitTxConstraintsWith @Void lookups tx
        void $ awaitTxConfirmed $ txId ledgerTx
        logInfo @String $ "collected gifts"
                
where we basically create two functionalities: `give` and `grab`.The first one allows one account to put tokens on the contract for another actor to grab it. 

2.4.1 `give` takes an Integer as input and maps it to a contract. It starts by hashing the datum (in this case there is no previous history so the consturctor `Datum` takes non-important inputs `0 []`) and amount of tokens. (in this case the tokens are ADA, we convert the `amount` into ADA with the hyperfunction `Ada.lovelaceValueOf`). Then, the transaction is submitted (`ledgerTx <- submitTx tx`) and a waiting for the logs to be completed follows.

2.4.2 `grab` allows for another actor to go and grab te tokens deposited by the giver. It starts by looking at all the UTXOs that are sitting on the giver plutus address that we cerated (`utxos <- utxoAt scrAddress`). Then we verify the constraints** (to be explained in latter lectures), for now we can say that all they do is to check the availability of funds. Remember that for a transaction to be completed we need a **Datum, Script, Redeemer**, we got the first one, so for the redeemer we embeed it into the transaction `tx`, in particular for this contract the inputs for the redeemer are non-important (`I 17`, where 17 is arbitraty), this means that anyone can grab the tokens. After this, the actions are submitted and will wait for confirmation.

#### 2.5 give options to wallets
To finalize, the last chunk of code

    endpoints :: Contract () GiftSchema Text ()
    endpoints = (give' `select` grab') >> endpoints
      where
        give' = endpoint @"give" >>= give
        grab' = endpoint @"grab" >>  grab

gives the options to the wallets to be able to chose which action to take, grab and/or give as manny times they want

#### 2.6 playground interface
The last chunk just allows the playground display

    mkSchemaDefinitions ''GiftSchema

    mkKnownCurrencies []



## 3. Explore the second contract, the [`Burn.hs`](https://github.com/Igodlab/plutus-pioneer-program/blob/main/code/week02/src/Week02/Burn.hs) contract
This contract makes few modifications to the `Gift.hs` contract, it basically removes the functionality of an actor to grab tokens deposited to a contract, so basically they are lost or burned.


This is done in the void-like function that stops the rest of the code to run because of lack of the monoid `()` and instead it prints an error in the logs.


    mkValidator : Data -> Data -> Data -> ()
    mkValidator _ _ _ = traceError "NO WAY!"
    
Also, we can point out that  `traceError` is a Plutus function that takes overloaded-plutus-strings (imported in languages `{-#LANGUAGE OverloadedStrings#-}`) whereas `error` is a Prelude function that takes a normal string as input. In any case, what it does is just include the message into the logs.


## 4. [`FortyTwo.hs`](https://github.com/Igodlab/plutus-pioneer-program/blob/main/code/week02/src/Week02/FortyTwo.hs) contract
So far our `Gift.hs` contract allowed anyone to be the redeemer of the tokens deposited to the contract. Now, we correct this by forcing the redeemer to be the *one who claims only 42 tokens* (redeemer will still grab all the tokens but he has to claim only 42). This is implemented  in the input of the the action

    type GiftSchema = 
        BlockchainActions
            .\/ Endpoint "give" Integer  -- takes integer
            .\/ Endpoint "grab" Integer  -- used to take (), now takes Integer
    ...
    ...
    ...
    grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => Integer -> Contract w s e ()
    grab r = do -- used to skip inputs, now takes r
        ...
        ...
        ...
            tx :: TxConstraints Void Void
            tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ I r | oref <- orefs]        
            
and logiaclly now the input of the redeemer constructor is `I r`.

But wait! we also have to modify some other details before deploying this. The void-like function has to be changed, now it can take cases 
    
    mkValidator :: Data -> Data -> Data -> ()
    mkValidator _ r _
        | r == I 42 = ()
        | otherwise = traceError "wrong redeemer"

this is called **guards** in Haskell, and it is a more readable way of coding cases. Lastly we have to change the endpoints

    endpoints :: Contract () GiftSchema Text ()
    endpoints = (give' `select` grab') >> endpoints
      where
        give' = endpoint @"give" >>= give
        grab' = endpoint @"grab" >>= grab -- used to be >> now its >>=

## 5. [`Typed.hs`](https://github.com/Igodlab/plutus-pioneer-program/blob/main/code/week02/src/Week02/Typed.hs)
This contract introduces a better way of writting the make validator. In all the codes above we have used `()` where everything runs if the input patterns are correct but for the contract to burn tokens (ommint underneath code) we rely on `()` to break! We can substancially improve this by using a Plutus function validator-context `ValidatorCtx` that returns a boolean instead.

    {-#INLINABLE mkValidaor#-}
    mkValidator :: () -> Integer -ValidatorCtx -> Bool
    mkValidator () r _
        | r == 42 = True
        | otherwise = False
        
now the inlinable function of the **template Haskell $$** won't run because it expects a `Data -> Data -> Data` type. This can be corrected using and advanced feature of Haskell named **typed**, this is advance but and often is repeated for the same type of solution.

    data Typed
    instance Scripts.ScriptType Typed where
        type instance DatumType Typed = ()
        type instance RedeemerType Typed = Integer
        
    inst :: Scripts.ScriptInstance Typed
    inst = Scripts.validator @Typed
        $$(PlutusTx.compile [|| mkValidator ||])
        $$(PlutusTx.compile [|| wrap ||])
      where
        wrap = Scripts.wrapValidator @() @Integer
        
    validator :: Validator
    validator = Scripts.validatorScript inst
    
    
More detail on how Plutus implements these on this advanced Haskell practices to move from typeclass to typeclass, visit the [PlutusTx](https://github.com/input-output-hk/plutus/blob/master/plutus-tx/src/PlutusTx/IsData/), more specifially the [`Class.hs`](https://github.com/input-output-hk/plutus/blob/master/plutus-tx/src/PlutusTx/IsData/Class.hs) file.

## 6. [`IsData.hs`](https://github.com/Igodlab/plutus-pioneer-program/blob/main/code/week02/src/Week02/IsData.hs)
The final example of this lecture is a modification of our previous `Typed.hs` contract. Here we will explore more on **custom-data-types**. We can create data types as we wish, do just as an example



    	
