# Week 02

 These is a written version of [Lecture #2](https://youtu.be/E5KRk5y9KjQ).

 It covers low-level, untyped on-chain validation scripts and high-level, typed on-chain validation scripts.

 Please feel free to raise pull requests for typos, poor formatting, poor grammar, or anything else that is poor or inaccurate.

 ## Overview

We saw in the first lecture that there are two sides to a smart contract - an on-chain part and an off-chain part.

The on-chain part is about validation. It allows nodes to validate a given transaction and whether it is allowed to consume a given UTxO.

The off-chain part lives in the user's wallet. It constructs and submits suitable transactions.

Both are important topics. We have to master both in order to write smart contracts, but for now we will concentrate on the on-chain part.

Let's recall the Extended UTxO model where the idea is that we introduce a new type of address. 

 ![alt text](img/1.png "Image 1")

In the simple UTxO model are so-called public key addresses, where the address is given by the hash of the public key. If a UTxO sits at such a public key address, then a transaction can consume that UTxO as an input if the signature belonging to that public key is included in the transaction.

What the (E)UTxO model does is extend this by adding script addresses that can run arbitrary logic.

When a transaction wants to consume a UTxO sitting at a script address is validated by a node, the node will run the script and then, depending on the result of the script, decide whether the transaction is valid or not.

And recall that there were three more additions:

1. Instead of just having signatures on transactions, we have so-called Redeemers - arbitrary pieces of data.
2. On the UTxO output side, we have an additional arbitrary piece of data called Datum, which you can think of as a little piece of state that sits on the UTxO.
3. Finally, we have the context. There are various choices of what this context can be. It can be very restrictive, consisting just of the Redeemer (as in Bitcoin), or very global, consisting of the whole state of the blockchain (as in Ethereum). In Cardano, it is the transaction that is being validated, including all its inputs and outputs.

So, there are three pieces of data that a Plutus script gets. The Datum, sitting at the UTxO, the Redeemer coming from the input and the validation, and the Context, consisting of the transaction being validated and its inputs and outputs.

In a concrete implementation like Plutus, these pieces of information need to be represented by a concrete data type - a Haskell data type. As it happens, the choice was made to use the same data type for all three of them. At least at the low-level implementation.

We will look at that first, but in real life noboby would actually use this low-level. There are more convenient ways to use more suitable data types for these things, and we will come to that later in this lecture.

## PlutusTx.Data

As mentioned, the Datum, Redeemer and Context share a data type. That data type is defined in the package *plutus-tx*, in the module [*PlutusTx.Data*](https://github.com/input-output-hk/plutus/blob/master/plutus-tx/src/PlutusTx/Data.hs). It is called, simply, *Data*. 

    data Data =
          Constr Integer [Data]
        | Map [(Data, Data)]
        | List [Data]
        | I Integer
        | B BS.ByteString
        deriving stock (Show, Eq, Ord, Generic)
        deriving anyclass (Serialise, NFData)

It has five constructors.

- *Constr* takes an Integer and, recursively, a list of *Data*
- *Map* takes a list of pairs of *Data*. You can think of this as a lookup table of key-value pairs where both the key and the value are of type *Data*
- *List* takes a list of *Data*
- *I* takes a single Integer
- *B* takes a Bytestring

For those familiar with the JSON format, this is very similar. The constructors are not exactly the same, but, like JSON, you can represent numbers, strings, lists of data and key-value pairs. It can represent arbitrary data, which makes it very suitable for our purpose.

We can also explore this type in the REPL.

From the plutus-pioneers-program repository. Remember that you may need to start a nix-shell from the Plutus repository before changing into the week02 directory.

    cd code/week02
    cabal repl

You should get a response like the following:

    Ok, 9 modules loaded.


From with the REPL:

    import PlutusTx
    :i Data

This will give information about the type *Data*.

    Prelude Week02.Burn> import PlutusTx.Data
    Prelude PlutusTx.Data Week02.Burn> :i Data
    type Data :: *
    data Data
      = Constr Integer [Data]
      | Map [(Data, Data)]
      | List [Data]
      | I Integer
      | B bytestring-0.10.12.0:Data.ByteString.Internal.ByteString
        -- Defined in ‘PlutusTx.Data’
    instance Eq Data -- Defined in ‘PlutusTx.Data’
    instance Ord Data -- Defined in ‘PlutusTx.Data’
    instance Show Data -- Defined in ‘PlutusTx.Data’
    Prelude PlutusTx.Data Week02.Burn> 

Now we can play with it. We can use the *I* constructor to create a value of type *Data*.

    Prelude PlutusTx.Data Week02.Burn> I 7
    I 7

We can ask for its type, and confirm that it is indeed of type *Data*:

    Prelude PlutusTx.Data Week02.Burn> :t I 7
    I 7 :: Data

The easiest way to create a value of type *Data* using the *B* constructor is to use the GHC Extension *OverloadedStrings*. This allows literal strings to be used in place of string-like data types and the compiler will interpret them as their intended type.

    Prelude PlutusTx.Data Week02.Burn> :set -XOverloadedStrings
    Prelude PlutusTx.Data Week02.Burn> :t B "Haskell"
    B "Haskell" :: Data

We can also use more complicated constructors, like Map and List:

    Prelude PlutusTx.Data Week02.Burn> :t Map [(I 7, B "Haskell"), (List [I 0], I 1000)]
    Map [(I 7, B "Haskell"), (List [I 0], I 1000)] :: Data

## Plutus Validator

Now we are ready to implement our very first Validator.

As we know, a validator is a script that takes three pieces of input - the Datum, the Redeemer and the Context, which, at the lowest level are represented by the *Data* data type.

### The Gift Contract (Gift.hs)

We start the script by copy pasting a list of GHC language extensions, plus some dependency imports.

    {-# LANGUAGE DataKinds           #-}
    {-# LANGUAGE FlexibleContexts    #-}
    ...

    module Week02.Gift where

    import           Control.Monad       hiding (fmap)
    import           Data.Map            as Map
    ...
    import           Text.Printf         (printf)

Then, we write the Validator. It is a Haskell function that takes three arguments, all of type *Data*.

    mkValidator :: Data -> Data -> Data -> ()

Somewhat surprisingly, the result of the function is (). This is the Haskell Unit type, similar to *void* in some other languages, like C++ or C# or Java - it's the type that carrys no information.

Unit is a built-in type in Haskell and it has just one value, which is written in the same way as the type itself, as we can see from the REPL.

    Prelude Week02.Gift> ()
    ()

A function with a return type of () is quite unusual in Haskell. In more mainstream languages, it is quite common for functions or procedures to return no value. In these situations, the functions are only important for their side-effects, such as a Java function that prints something to the console.

But Haskell is a pure language. If you want side-effects, this will be shown by the type system. For example if the mkValidator were to perform any IO, it would have a type signature of:

    mkValidator :: Data -> Data -> Data -> IO ()

This would indicate a function that performs IO side-effects but has no interesting return value.    

But, as we know that the real mkValidator function performs no side-effects and returns no value, there is really nothing useful that it can do.

However, there is something that the function can do as well as returning (), namely it can throw an exception or have an error. And that's what Plutus uses.

The idea is that if the mkValidator function does not run into an error or throw an exception, then validation succeeds. If it throws an error then validation fails and the transaction is rejected.

Let's go back into the REPL and load the Gift module.

    Prelude Week02.Burn> :l src/Week02/Gift.hs 
    Ok, one module loaded.








