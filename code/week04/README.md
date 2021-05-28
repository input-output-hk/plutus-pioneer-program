# Week 04

These is a written version of [Lecture #4](https://youtu.be/6Reuh0xZDjY).

In this lecture we learn about Monads. In particular the EmulatorTrace and Contract monads.

Please feel free to raise pull requests for typos, poor formatting, poor grammar, or anything else that is poor or inaccurate.

## Overview

We have spent the last two lectures talking about the on-chain part of Plutus - the validation logic that is compiled to Plutus script and actually lives on the blockchain and is executed by nodes that validate a transaction.

There is a lot more to say about that on-chain part.

We haven't looked at more complex examples of validation yet that make more sophisticated use of the context, and we haven't see how native tokens work, yet (Plutus script is also used to validate the minting and burning of native tokens).

We will definitely have to talk about those topics, and come back to that.

However, before we go into too many sophisticated topics of on-chain validation, we mustn't neglect the off-chain part, because it is equally important.

The on-chain part takes care of validation but, in order for there to be something to be validated, we must build a transaction and submit it to the blockchain. And, that is what the off-chain part does.

So, we will start talking about how to write off-chain Plutus code.

Unfortunately there is a slight problem concerning the Haskell features needed.

The on-chain part that we have seen so far is somewhat alien and takes a little getting used to, due to the fact that we have the additional complication of the compilation to Plutus script. But, we don't really have to worry about that if we use the Template Haskell magic.  In that case the validator function is just a plain function. 

And it is actually a very simple Haskell function from the technical point of view. We don't use any fancy Haskell features to write this function.

One of the reasons for that is the way Plutus compilation works. We have seen how, in order for the compilation to Plutus to succeed, all the code used by the validation function must be available within the Oxford Brackets. This means that all the functions relied on by the *mkValidator* function must use the INLINABLE pragma.

```haskell
{-# INLINABLE mkValidator #-}
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ _ _ = ()

$$(PlutusTx.compile [|| mkValidator ||])
```

And recall, that because the standard Haskell functions don't have this INLINABLE pragma, there is a new Plutus Prelude module that is similar to the standard Haskell Prelude, but with the functions defined with the INLINABLE pragma.

But, of course, there are hundreds of Haskell libraries out there and most of them weren't written with Plutus in mind, so we can't use them inside validation. And, that has the effect that the Haskell inside validation will be relatively simple and won't have many dependencies.

## Monads

In the off-chain part of Plutus, the situation is reversed. We don't have to worry about compilation to Plutus script - it is just plain Haskell. But, the flip side is that, the way this is implemented, it uses much more sophisticated Haskell features - e.g. so-called effect systems, streaming and, in particular, monads.

All the off-chain code (the wallet code), is written in a special monad - the Contract Monad.

Monads are infamous in the Haskell world. It is normally the first stumbling block for beginning Haskell coders.

There are a lot of tutorials out there that try to explain Monads. Monads get compared to burritos, and all sorts of metaphors are employed to try to explain the concept. But here, let's at least try to give a crash course in monads for those who are new to Haskell.

### Introduction to Monads

Before we get to general monads, we will start with *IO*, which is how IO side-effects are handled in Haskell. But, before we get to Haskell, let's look at a mainstream language like Java.

Let's look at the following Java method.

```java
public static int foo() {
    ...
}
```

This function takes no arguments, and it returns an *int*. Let's imagine it gets called twice in the code.

```java
...
final int a = foo();
...
final int b = foo();
```

Now, we note that, so long as we don't know what is going on inside the foo() function, the return value of the following expression is unknown.

```java
a == b; // true or false? at compile time, we don't know
```

We do not know if *a* is the same as *b* because, in Java, it is perfectly possible that some IO happens inside *foo()*. For example, there code be code that asks the user to enter input on the console and uses this to compute the return value.

This means that, in order to reason about the code, we need to look inside *foo()*, which makes testing, for example, more difficult. And it means that, it the first call to *foo()* returns, for example, 13 - we cannot just replace all other calls to *foo()* with the known return value of 13.

In Haskell the situation is very different because Haskell is a pure functional language. The equivalent signature in Haskell would be something like:

```haskell
foo :: Int
foo = ...
```

Now, if we have a situation where we call *foo* twice, even though we don't know what the value of *foo* is, we know for sure that the two return values will be the same.

```haskell
let a = foo
let b = foo

if a == b
  then ... -- we know this to be true
  else ... -- the compiler could tell you here not to waste your typing
```

This is a very important feature that is called *referential transparency*. There are, in fact, some escape hatches to get around this, but we can ignore this.

This makes tasks such as refactoring and testing much easier.

This is all very well, but you need side-effects in order to have an effect on the world. Otherwise, all your program does is heat up the processor.

You need input and output. You must be able to write output to the screen, or read input from the keyboard, or a network connection, or a file, for example.

There is a famous [video by Simon Peyton-Jones called Haskell Is Useless](https://www.youtube.com/watch?v=iSmkqocn0oQ) which explains that it is beautiful mathematically to have a pure, side effect-free language, but in the end you do need side effects to make anything happen.

And Haskell does have a way to handle side effects and that is the IO Monad. But, don't worry about the monad part just yet.























