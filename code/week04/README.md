# Lecture 4 notes
Lecture notes based on the first ever official [Plutus-Pioneer program](https://github.com/input-output-hk/plutus-pioneer-program). This notes follow the [YouTube lecture 4](https://www.youtube.com/watch?v=FUglEDP5brI&t=7116s).


## 0. Review of Monads

This week's class starts with a nice review on Monads. Because they are a very powerful concept in Haskell for mapping functions over different datatypes it comes with no surprise that they run on many operations in Cardano wallets.

A very important monad in Plutus is the **Contract Monad** ([]()) which defines code that will run in a wallet. It defines the off-chain part in Plutus.

## 1. Important Funtctions in the Plutus repo

*Emulator Trace Monad* ([`Emulator.hs`](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Trace/Emulator.hs)) enables to recreate conditions in a local, less polished, terminal compared to the playground. Some relevant lines of the code mentioned in the lecture are (lines 224-235)

    -- | Run an emulator trace to completion, returning a tuple of the final state
    -- of the emulator, the events, and any error, if any.
    runEmulatorTrace
        :: EmulatorConfig
        -> EmulatorTrace ()
        -> ([EmulatorEvent], Maybe EmulatorErr, EmulatorState)
    runEmulatorTrace cfg trace =
        (\(xs :> (y, z)) -> (xs, y, z))
        $ run
        $ runReader ((initialDist . _initialChainState) cfg)
        $ foldEmulatorStreamM (generalize list)
        $ runEmulatorStream cfg trace
        
Where `runEmulatorTrace` allows the wallet to trace some logs. It is a pure function. The `EmulatorConfig` data-type is defined in a different directory: [`Stream.hs`](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Wallet/Emulator/Stream.hs) (lines 135-140)

    data EmulatorConfig =
        EmulatorConfig
            { _initialChainState      :: InitialChainState -- ^ State of the blockchain at the beginning of the simulation. Can be given as a map of funds to wallets, or as a block of transactions.
            } deriving (Eq, Show)

    type InitialChainState = Either InitialDistribution Block -- line 140

As we can see    the `EmulatorConfig` is a single **record-type** named `InitialChainState`, which is either an `InitialDistribution` or `Block` (line 140) defined in another module [`Trace.hs`](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Contract/Trace.hs) as shown below (line 115):

    type InitialDistribution = Map Wallet Value  -- line 115
    .
    .
    .
    defaultDist :: InitialDistribution
    defaultDist = defaultDistFor allWallets

    defaultDistFor :: [EM.Wallet] -> InitialDistribution
    defaultDistFor wallets = Map.fromList $ zip wallets (repeat (Ada.lovelaceValueOf 100_000_000))

which maps a wallet to a value as we would expect. It is importntn to note that a value is not limited to ADA value but also NFTs and Native Tokens.

## 2. Trace Logs 

Here we start playing with the logs that are printed in the output of the info of a wallet. To test all of this we will be using the repl in the week04 folder. 

i) Start importing the modules we saw in **1. Important Functions in the Plutus repo**

    repl$ import Plutus.Trace.Emulator
    repl$ import Plutus.Contract.Trace
    
we can test some default states/distributions of wallets by typing

    repl$ defaultDist                           -- lists 10 initial wallets by default
    repl$ defaultDistFor [Wallet 2, Wallet 11]  -- shows wallets 2 & 11
      
ii) Now we can run the `runEmulatorTrace` and for this we need import the wallet-emulator-stream which is where its data-type is defined as seen in Section 1. 

    repl$ import Wallet.Emulator.Stream
    repl$ runEmulatorTrace (EmulatorConfig $ Left defaultDist) $ return ()
    
Ooops! we get a lot of info printed on the termminal, which is not very useful for humans to read. To get something more readable we have to filter info. We can use another function to print on terminal, this is the `runEmulatorTraceIO` and like `runEmulatorTrace` is defined in [`Emulator.hs`](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Trace/Emulator.hs). So lets try

    repl$ runEmulatorTraceIO $ return ()
    
we can see that information is printed in a much more controled way. If we would like even more control the function `runEmulatorTRaceIO'` allows to filter info usign **Just/Maybe** functionalities.

## 3. More Interesting Traces [`Trace.hs`](https://github.com/Igodlab/plutus-pioneer-program/blob/main/code/week04/src/Week04/Trace.hs)

Lets import [`Vesting.hs`](https://github.com/Igodlab/plutus-pioneer-program/blob/main/code/week03/src/Week03/Vesting.hs) code from week03 into a new file named `Trace.hs`. Here we want to add a trace to what we have done in the plyground for the `Vesting.hs` code. We can print the log instances just like in the playground using

    repl$ :l ./src/Week04/Trace.hs
    repl$ test
    .
    .
    .
    Slot 00021: *** USER LOG: reached slot Slot {getSlot = 21}
    .
    .
    .
    
So we can see that the `** USER LOG` was printed on screen, this shows that we can log in both from the contract and from the trace.
    
## 4. Contract Monad [`Contract.hs`](https://github.com/Igodlab/plutus-pioneer-program/blob/main/code/week04/src/Week04/Contract.hs)

The purpose of the **Contract-Monad** is to define off-chain code that runs in the wallet. Notice that it has four type-parameters `Contraact w s e a` where i) `w` is the type input that allows to write log messages of type w. ii) `s`, describes the blockchain capabilities, iii) `e` describes the type of error messages & iv) `a` . This is illustrated 

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
-- Contract w s e a
-- EmulatorTrace a

myContract1 :: Contract () BlockchainActions Text ()
myContract1 = do
    void $ Contract.throwError "BOOM!"
    Contract.logInfo @String "Hello from the contract!"

myTrace1 :: EmulatorTrace ()
myTrace1 = void $ activateContractWallet (Wallet 1) myContract1

test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1
```

where i) `Contract ()` is a void unit because in this particular case we are not interested in any logs. ii) `BlockchainActions` allows actions like submiting a transaction, waiting slot time... However, an exception is that it doesn't handle is endpoints. iii) for error messages we use `Text ()` that allows overloaded strings.

