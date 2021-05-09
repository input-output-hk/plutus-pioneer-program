# Lecture 4 notes
Lecture notes based on the first ever official [Plutus-Pioneer program](https://github.com/input-output-hk/plutus-pioneer-program). This notes follow the [YouTube lecture 3](https://www.youtube.com/watch?v=FUglEDP5brI&t=7116s).


## 0. Review of Monads

This week's class starts with a nice review on Monads. Because they are a very powerful concept in Haskell for mapping functions over different datatypes it comes with no surprise that they run on some operations in Cardano wallets.

A very important monad in Plutus is the *Contract Monad* ([]()) defines code that will run in a wallet. It defines the off-chain part in Plutus.

*Emulator Trace Monad* ([`Emulator.hs`](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Trace/Emulator.hs)) enables to sumilate conditions in a local less polished terminal compared to the playground. Some relevant lines of the code mentioned in the lecture are (lines 224-235)

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
        
Where `runEmulatorTrace` allows the wallet to trace some logs. It is a pure function. The `EmulatorConfig` data-type is defined in a different directory: [`Stream.hs`](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Wallet/Emulator/Stream.hs) (lines 135-138)

    data EmulatorConfig =
        EmulatorConfig
            { _initialChainState      :: InitialChainState -- ^ State of the blockchain at the beginning of the simulation. Can be given as a map of funds to wallets, or as a block of transactions.
            } deriving (Eq, Show)
    
    type InitialChainState = Either InitialDistribution Block

We can see that the emilator-config is a single record-type named `InitialChainState`, which is ether a distribution or block defined in another module (`Trace.hs`)[https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Contract/Trace.hs] (line 115) 

    type InitialDistribution = Map Wallet Value  -- line 115
    .
    .
    .
    defaultDist :: InitialDistribution
    defaultDist = defaultDistFor allWallets

    defaultDistFor :: [EM.Wallet] -> InitialDistribution
    defaultDistFor wallets = Map.fromList $ zip wallets (repeat (Ada.lovelaceValueOf 100_000_000))

which maps a wallet to a value



