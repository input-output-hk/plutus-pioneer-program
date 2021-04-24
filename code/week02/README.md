# Lecture 2 notes
Lecture notes based on the first ever official [ Plutus-Pioneer program](https://github.com/input-output-hk/plutus-pioneer-program). This notes follow the [YouTube lecture 2](https://www.youtube.com/watch?v=E5KRk5y9KjQ/).

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

We will brakdown some of the sections of the code that give the functionality to the contract. In the `Gift.hs` contract
    
    mkValidator :: Data -> Data -> Data -> ()
    mkValidator _ _ _ = ()
    
Which basically is a Haskell function (`mkValidator`), that latter on will be compiled into a Plutus function. It just creates a function that regardless of the inputs proceeds to the reading of the following lines of the code. The first goal is to create a validator (`mkValidatorScript`), this one uses **template Haskell**  this basically compiles inline (allowed by the `{~#INLINABLE mkValidator#~}` pragma at line 30) everything after the *splice* (after double dollar sign `$$`) in this case it will invoke the compilation of `PlutusTx.compile` with its input`mkValidator` (this is whatever is inside the double bars, inside the square brakets).

    validator :: Validator
    validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])
    
Next, we create a validator hash to then turn in into an actual address (not a public key address yet, this is a Plutus address)

    valHash :: Ledger.ValidatorHash
    valHash = Scripts.validatorHash validator
    
    scrAddress :: Ledger.Address
    scrAdress = ScriptAdress valHash
    
In order to try this we need wallet code as follows below

    typeGiftSchema = 
        BlockchainAction
            .\/ Endpoint "give" Integer
            .\/ Endpoint "grab" ()
            
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
                
where we basically create two functionalities: `give` and `grab`.The first one allows one account to put modey on the contract for another actor to grab it. `give` takes an Integer as input and mpas it to a contract. 

## 3. Explore the second contract, the ['Burn.hs`](https://github.com/Igodlab/plutus-pioneer-program/blob/main/code/week02/src/Week02/Burn.hs)   


