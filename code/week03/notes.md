# Notes Week3

## Setup checkout plutus commit from cabal.project file
219992289c6615e197069d022735cb4059d43229

inside cabal repl shell

## Interval comparison
`import Plutus.V1.Ledger.Interval`
### interval Smart Constructor
`interval (10 :: Integer) 20`
### member
is 9 a member of the intervall
`member 9 $ interval (10 :: Integer) 20`
### from and to
intervall to positive infinite from a given Integer
`from (10 :: Integer)`
intervall from negative infinite to a given Integer
`to (10 :: Integer)`

### intersection
"Schnittmenge" between two intevalls

`intersection (interval (10 :: Integer) 20) $ interval (5 :: Integer) 10`

*Question*: If i want an intersection between an interval from 10 to 20 and 5 to 8 (the two intervals are not overlapping), i would expect an empty set. Instead i get an interval from lowerBound=10 to upperBound=8. Why is this?

### contains
is the interval a fully contained in interval b
`contains (to (100 :: Integer)) $ interval 30 80`

### overlaps
are the two intervals overlapping
`overlaps (interval (10 :: Integer) 20) $ (interval 5 8)`

## Find the PubKeyHash of a Wallet
`import Ledger`
`import Wallet.Emulator`

### Read the pubKeyhash of Wallet 2
`pubKeyHash $ walletPubKey $ Wallet 2`

## POSIX Time of a Slot
`import Ledger.TimeSlot`

`slotToBeginPOSIXTime :: SlotConfig -> Slot -> POSIXTime`
Usually the Default Slot Config is used
`import Data.Default`

dev is not the default SlotConfig therefore it can be used
`slotToBeginPOSIXTime def 10`


