# Week 07 Notes

This notes follow the seventh lecture from [Lars YouTube](https://www.youtube.com/watch?v=oJupInqvJUI&t=839s). 

The main topics of this lecture is to introduce state machines for us to see how powerful and convenient they can be. This is exemplified in this lecture's app: a simple binary game:

Alice & Bob are playing a binary choice game, if both choose the same number then Alice wins and otherwise Bob wins.

When Alice chooses a number ([0,1] + nonce) hashes her chooice and sends it to Bob. Then Bob knows the hash but does not know Alice's choice so he can not cheat. Then Bob makes his choice [0.1] and sends it raw to Alice, at this point Alice already knows if she has won/lost but to prevent her from cheating now she sends her raw choice ([0,1] + nonce). Finally Bob checks that the hash of it matches the initial hashed message. 

| Action | Alice | Bob |
| --- | --- | --- |
| A turn | hash(choice ++ nonce) | waits |
| B turn | waits | choice |
| A confirms | (choice ++ nonce) | verifies hash |
| result | win/loose | win/loose |

## 1. EvenOdd

The first implementation of the game is a bit verbose, it only makes use of Plutus and Haskell functions and follows the raw logic.

- `data Game` -> type that contains information about the steps of the game (much like the table above) plus it adds varaibles for deadlines.
- `data GameDatum` -> type that concatenates a bytestring 

## State Machines

A state machine is basically a system that reacts to inputs by transitioning to other states, it is represented as a directed graph which transitions state. Eg. in our diagram for the Alice/Bob game

```
             ______              __________
--Alice-->  | Hasb |  --Bob-->  | hasb, cb |----------|
            |______|    play    |__________|          |
                  \                   |               |
                   \                  |           Bob | claim
              Alice \ claim     Alice | reveal        |
                     \                |               /
                      \               v              /
                       \        _____________       /
                        \--->  | Final state |  <--/
                               |_____________|

```                               
