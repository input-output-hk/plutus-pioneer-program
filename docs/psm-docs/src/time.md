# How to work with time

Note that every time we submit block successfully one slot passes.
By default, one slot lasts for 1 second. Sometimes we want to check for TX that should
happen in the future or after some time passes.

For that we can use functions to make blockchain move forward:

```haskell
waitNSlots :: Slot -> Run ()
wait       :: POSIXTime -> Run ()
waitUntil  :: POSIXTime -> Run ()
```

`waitNSlots` waits in slots while `wait` and `waitUntil` wait in `POSIXTime` (counted in milliseconds).
Also we can query current time with functions:

```haskell
currentSlot :: Run Slot
currentTime :: Run POSIXTime
```

By default, we always start at the beginning of UNIX epoch. Start of blockchain is set to 0 posix millis.
Closely related is function `validateIn`. It sets the valid range for TX:

```haskell
validateIn :: POSIXTimeRange -> Tx -> Run Tx
```

The result is wrapped in the Run-monad because we convert 
`POSIXTime` to `Slot`'s under the hood and conversion depends on blockchain config.

Note that if current time of blockchain is not included in the Tx it will be rejected.
So we can not submit TX that is going to be valid in the future. We rely on property
that all TXs are validated right away. It means  that current time should be included in valid range for TX.
By default, it's `always`, it means "any time" and should work. 

To specify time interval relative to the current time we can use functions:

```haskell
currentTimeInterval :: POSIXTime -> POSIXTime -> Run POSIXTimeRange
currentTimeInterval minT maxT
```

It creates interval of `[currentTime + minT, currentTime + maxT]`
So to work properly minT should be negative and maxT should be positive.

also we have a function that creates time interval with equal radius around current time:

```haskell
currentTimeRad :: POSIXTime -> Run POSIXTimeRange
currentTimeRad rad
```

it creates interval of `[currentTime - rad, currentTime + rad]`.

Also note that because Plutus uses `POSIXTime` while the Cardano network uses Slots, the inherent
difference in precision between Slot and `POSIXTime` may cause unexpected validation failures.
For example, if we try to build a transaction using the `POSIXTimeRange` `(1100,3400)`, it will be converted to
`SlotRange` `(1,4)` to go through the Cardano network, and when it is converted back to Plutus, the POSIXRange
will have been extended to cover the whole slot range, becoming `(1000,4000)` and maybe trespassing
the allowed limits set by the validator.

POSIXTime is counted in milliseconds.
To count in human-readable format we have convenient functions: `days`, `hours`, `minutes`, `seconds`:

```haskell
wait $ hours 4
```

That's it. you can find complete example at the test suite (see `Suites.Plutus.Model.Script.Test.Game`).
There are other useful function to dicuss. Look up the docs for the `Mock` and `Contract` modules.


