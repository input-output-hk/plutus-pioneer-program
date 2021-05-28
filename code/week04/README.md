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

Here is how we do it in Haskell.

```haskell
foo :: IO Int
foo = ...
```

*IO* is a type constructor that takes one argument, like some other examples of type constructors such as *Maybe* and *List*. However, unlike those examples, *IO* is special, in the sense that you can't implement it in the language itself. It is a built-in primitive.

The return value *IO Int* tells us that this is a recipe to compute an *Int*, and this recipe can cause side effects. A list of instructions telling the computer what to do in order to end up with an *Int*.

It is important to notice that referential transparency is not broken here. The result of the evaluation of *foo* is the recipe itself, not the *Int* value. And as the recipe is always the same, referential transparency is maintained.

The only way to actually execute such a recipe in a Haskell program is from the main entry point of the program - the *main* function. You can also execute *IO* actions in the REPL.

### Hello World - putStrLn

Hello World in Haskell looks like this:

```haskell
main :: IO ()
main = putStrLn "Hello, world!"
```

Here, *main* is a recipe that performs some side effects and returns Unit - nothing of interest.

Let's look at *putStrLn* in the REPL. We see that it is an IO action that takes a *String* and returns no interesting result.

```haskell
Prelude Week04.Contract> :t putStrLn
putStrLn :: String -> IO ()

Prelude Week04.Contract> :t putStrLn "Hello, world!"
putStrLn "Hello, world!" :: IO ()
```

We can also run this. Open up the app/Main.sh file and edit the *main* function so it reads:

```haskell
main :: IO ()
main = putStrLn "Hello, world!"
```

Then run

```bash
cabal run hello
```

We will take a quick look at the cabal file now.

In previous lectures we only needed the *library* section in the *plutus-pioneer-program-week04.cabal* file as we were dealing only with library functions. Now, we need to add an *executable* stanza.

```cabal
executable hello
  hs-source-dirs:      app
  main-is:             hello.hs
  build-depends:       base ^>=4.14.1.0
  default-language:    Haskell2010
  ghc-options:         -Wall -O2
```

This specifies the source directory and which file holds the main function. Normally the file name must match the module name, but the *main* is an exception.

Rather than just asking for the type of *putStrLn*, we can run it in the REPL. As mentioned, the REPL allows us to execute IO actions.

```haskell
Prelude Week04.Contract> putStrLn "Hello, world!"
Hello, world!
```

### getLine

Let's look at *getLine*

```haskell
Prelude Week04.Contract> :t getLine
getLine :: IO String
```

This shows that it is a recipe, possibly producing side-effects, that, when executed will produce a *String*. In the case of *getLine*, the side-effect in question is that it will wait for user input from the keyboard.

If we execute *getLine* in the REPL.

```haskell
Prelude Week04.Contract> getLine
```

It waits for keyboard input. Then, if we enter something, it returns the result.

```haskell
Haskell
"Haskell"
```

There are a variety of IO actions defined in Haskell to do all sorts of things like reading files, writing files, reading from and writing to sockets.

But no matter how many predefined actions you have, that will never be enough to achieve something complex, so there must be a way to combine these primitive, provided IO actions into bigger, more complex recipes.

One thing we can do is make use of the *Functor* type instance of IO. Let's look at the type instances of *IO* in the REPL.

```haskell
Prelude Week04.Contract> :i IO
type IO :: * -> *
newtype IO a
  = ghc-prim-0.6.1:GHC.Types.IO (ghc-prim-0.6.1:GHC.Prim.State#
                                   ghc-prim-0.6.1:GHC.Prim.RealWorld
                                 -> (# ghc-prim-0.6.1:GHC.Prim.State#
                                         ghc-prim-0.6.1:GHC.Prim.RealWorld,
                                       a #))
  	-- Defined in ‘ghc-prim-0.6.1:GHC.Types’
instance Applicative IO -- Defined in ‘GHC.Base’
instance Functor IO -- Defined in ‘GHC.Base’
instance Monad IO -- Defined in ‘GHC.Base’
instance Monoid a => Monoid (IO a) -- Defined in ‘GHC.Base’
instance Semigroup a => Semigroup (IO a) -- Defined in ‘GHC.Base’
instance MonadFail IO -- Defined in ‘Control.Monad.Fail’
```

We see the dreaded *Monad* instance, but we also see a *Functor* instance. *Functor* is a very important type class in Haskell. If we look at it in the REPL:

```haskell
Prelude Week04.Contract> :i Functor
type Functor :: (* -> *) -> Constraint
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
  	-- Defined in ‘GHC.Base’
instance Functor (Either a) -- Defined in ‘Data.Either’
instance Functor [] -- Defined in ‘GHC.Base’
instance Functor Maybe -- Defined in ‘GHC.Base’
instance Functor IO -- Defined in ‘GHC.Base’
instance Functor ((->) r) -- Defined in ‘GHC.Base’
instance Functor ((,,,) a b c) -- Defined in ‘GHC.Base’
instance Functor ((,,) a b) -- Defined in ‘GHC.Base’
instance Functor ((,) a) -- Defined in ‘GHC.Base’
```

The important method here is *fmap*. The second function *(<$)* is a convenience function.

```haskell
fmap :: (a -> b) -> f a -> f b
```

This function, *fmap*, that all *Functor*s have tells us that, if we give it has access to a function that can turn an *a* into a *b*, then it can turn an *f a* into an *f b* for us. Here, we are interested in the case where *f* is *IO*.

If we specialized the function for *IO*, we would have a function like:

```haskell
fmap' :: (a -> b) -> IO a -> IO b
```

How does that work. Well, *IO a* is a recipe that has side effects and produces an *a*. So, how do we get a *b* out of that? We perform the recipe, but, before return the *a*, we apply the *(a -> b)* function to to *a* and return the result, which is the *b*.

In the REPL, let's look at the *toUpper* function.

```haskell
Prelude Week04.Contract> import Data.Char
Prelude Data.Char Week04.Contract> :t toUpper
toUpper :: Char -> Char
Prelude Data.Char Week04.Contract> toUpper 'q'
'Q'
```

If we want to apply that to a *String* rather than a *Char* we can use the *map* function. *String*s in Haskell are just lists of *Char*s.

```haskell
Prelude Data.Char Week04.Contract> map toUpper "Haskell"
"HASKELL"
```

The *map toUpper* function is a function from *String* to *String*.

```haskell
Prelude Data.Char Week04.Contract> :t map toUpper
map toUpper :: [Char] -> [Char]
```

And we can use this in combination with *fmap*. If we use *map toUpper* as our function to convert an *a* to a *b*, we can see what the type of output of *fmap* would be when applied to an *IO a*.

```haskell
Prelude Data.Char Week04.Contract> :t fmap (map toUpper) getLine
fmap (map toUpper) getLine :: IO [Char]
```

Let's see it in action.

```haskell
Prelude Data.Char Week04.Contract> fmap (map toUpper) getLine
haskell
"HASKELL"
```

We can also use the *>>* operator. This chains two *IO* actions together, ignoring the result of the first. In the following example, both actions will be performed in sequence.

```haskell
Prelude Week04.Contract> putStrLn "Hello" >> putStrLn "World"
Hello
World
```

Here, there is no result from *putStrLn*, but if there were, it would have been ignored. Its side effects would have been performed, its result ignored, then the second *putStrLn* side effects would been performed before returning the result of the second call.

Then, there is an important operator that does not ignore the result of the first *IO* action, and that is called *bind*. It is written as the *>>=* symbol.

```haskell
Prelude Week04.Contract> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

We see the *Monad* constraint, but we can ignore that for now and just think of *IO*.

What this says is that if I have a recipe that performs side effects then gives me a result *a*, and given that I have a function that takes an *a* and gives me back a recipe that returns a *b*, then I can combine the recipe *m a* with the recipe *m b* by taking the value *a* and using it in the recipe that results in the value *b*.

An example will make this clear.

```haskell
Prelude Week04.Contract> getLine >>= putStrLn
Haskell
Haskell
```

Here, the function *getLine* is of type *IO String*. The return value *a* is passed to the function *(a -> m b)* which then generates a recipe *putStrLn* with an input value of *a* and an output of type *IO ()*. Then, *putStrLn* performs its side effects and returns *Unit*.

There is another, very important, way to create *IO* actions, and that is to create recipes that immediately return results without performing any side effects.

That is done with a function called *return*.

```haskell
Prelude Week04.Contract> :t return
return :: Monad m => a -> m a
````

Again, it is general for any Monad, we only need to think about *IO* right now.

It takes a value *a* and returns a recipe that produces the value *a*. In the case of *return*, the recipe does not actually create any side effects.

For example:

```haskell
Prelude Week04.Contract> return "Haskell" :: IO String
"Haskell"
```

We needed to specify the return type so that the REPL knows which Monad we are using:

```haskell
Prelude Week04.Contract> :t return "Haskell" :: IO String
return "Haskell" :: IO String :: IO String

Prelude Week04.Contract> :t return "Haskell"
return "Haskell" :: Monad m => m [Char]
```

If we now go back to our *main* program, we can now write relatively complex *IO* actions.
























