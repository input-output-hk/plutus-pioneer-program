-- test code for the Either.hs

-- The idea of this file is to create a Monad that takes three integers typed as strings 
-- and then returns Just the sum of them if they are pure integers contained in the strings
-- otherwise it has to return a specific error message on where the parse error happened
module Week04.EitherTest where

import Text.Read (readMaybe)
import Week04.Monad -- imported for usage in sumThree''

-- create custom readEither
readEither :: (Read a) => String -> Either String a
readEither s = case readMaybe s of 
    Nothing -> Left $ "can not parse: " ++ s
    Just a -> Right a
    
sumThree :: String -> String -> String -> Either String Int
sumThree a b c = case readEither a of
    Left err -> Left err
    Right l -> case readEither b of
        Left err -> Left err
        Right m -> case readEither c of
            Left err -> Left err
            Right n -> Right (l+m+n)
            
-- simplifiy using binding
bindEither :: Either String a -> (a -> Either String b) -> Either String b
bindEither (Left err) _ = Left err
bindEither (Right x) f = f x

sumThree' :: String -> String -> String -> Either String Int
sumThree' a b c = readEither a `bindEither` \l ->
                  readEither b `bindEither` \m ->
                  readEither c `bindEither` \n ->
                  Right (l+m+n)
                  
-- simplify even more using import Monad.hs
sumThree'' :: String -> String -> String -> Either String Int
sumThree'' a b c = threeInts (readEither a) (readEither b) (readEither c)
