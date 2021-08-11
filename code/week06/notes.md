# Notes Week 6 Oracles

Commit hash for Week 6
8a20664f00d8f396920385947903761a9a897fe0

# Oracles
Oracles in the Blockchain World are Datasources from the outside World.

An Oracle is represented as an UTXO and we identify the correct Oracle by a NFT.
The Value of an Oracle is the Datum.


# Last
`import Data.Monoid (Last (..))`
Last is a wrapper arround Monoid which always remembers the last Just Value
for example if two Lasts are combined
`Last (Just 'x') <> Last (Just 'y')`
Will return `Just 'y'`

# List Comprehension
`[o | i <- txInfoInputs info, let o = tx`

## PAB Plutus application backend


# Play with oracles
all the executables are defined in the .cabal file and can therefore be referenced directly from week06 folder
1. Start PAB `cabal run oracle-pab` (starts Plutus application backend)
2. Start oracle Client `cabal run oracle-client` (Starts client which fetches the exchange rate)
3. Interact with swap client
 * 


