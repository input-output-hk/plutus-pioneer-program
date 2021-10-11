-- test code for the Writer.hs file

-- The idea of this file is to create a Monad that takes three custom type class typed as Writer
-- and then returns Just the sum of them if they are pure integers contained in the strings
-- otherwise it has to return a specific error message on where the parse error happened
--
module Week04.WriterTest where

import Control.Monad
import Week04.Monad -- imported for usage in sumThree''

-- create custom-type
data Writer a = Writer a [String] deriving (Show)

-- create function to convert from integer to Writer type
number :: Int -> Writer Int
number n = Writer n $ ["numero: " ++ show n]

-- create function to execute void
tell :: [String] -> Writer ()
tell = Writer ()

-- create function that sums three Writer numbers
sumThree :: Writer Int -> Writer Int -> Writer Int -> Writer Int
sumThree (Writer ai as) (Writer bi bs) (Writer ci cs) =
    let s = ai+bi+ci
        Writer _ ss = tell ["sum: " ++ show s]
    in Writer s $ as++bs++cs++ss
    
-- simplifiy this using binding
bindWriter :: Writer a -> (a -> Writer b) -> Writer b
bindWriter (Writer a as) f = 
  let
    Writer b bs = f a
  in
    Writer b $ as ++ bs


sumThree' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
sumThree' a b c = a `bindWriter` \l ->
                  b `bindWriter` \m ->
                  c `bindWriter` \n ->
                  tell ["sum: " ++ show (l+m+n)] `bindWriter` \_ -> Writer (l+m+n) []
                  
-- simplify even more using import Monad.hs
sumThree'' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
sumThree'' a b c = do
    s <- threeInts a b c
    tell ["sum: " ++ show s]
    return s
    
instance Functor Writer where
    fmap = liftM

instance Applicative Writer where
    pure = return
    (<*>) = ap

instance Monad Writer where
    return a = Writer a []
    (>>=) = bindWriter
