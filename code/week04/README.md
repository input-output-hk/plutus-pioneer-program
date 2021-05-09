# Lecture 4 notes
Lecture notes based on the first ever official [Plutus-Pioneer program](https://github.com/input-output-hk/plutus-pioneer-program). This notes follow the [YouTube lecture 3](https://www.youtube.com/watch?v=FUglEDP5brI&t=7116s).


## 0. Review of Monads

This week's class starts with a nice review on Monads. Because they are a very powerful concept in Haskell for mapping functions over different datatypes it comes with no surprise that they run on some operations in Cardano wallets.

A very important monad in Plutus is the *Contract Monad* defines code that will run in a wallet. It defines the off-chain part in Plutus.

*Emulator Trace Monad* (['Emulator.hs'](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Trace/Emulator.hs)) enables to sumilate conditions in a local less polished terminal compared to the playground. The relevant codes are 
