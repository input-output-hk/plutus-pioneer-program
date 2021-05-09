-- test file for te orginal Maybe.hs file
--
-- The idea here is to construct a Monad that takes three integers written as strings
-- then the monad allows the code to return Nothing if the string is not a pure integer
-- and returns Just sum of the integers otherwise
module Week04.MaybeTest where

import Text.Read (readMaybe)
import Week04.Monad -- imported for sumThree''

sumThree :: String -> String -> String -> Maybe Int
sumThree a b c = case readMaybe a of
    Nothing -> Nothing
    Just l -> case readMaybe b of
        Nothing -> Nothing
        Just m -> case readMaybe c of
            Nothing -> Nothing
            Just n -> Just (l+m+n)         

-- simplifiy this using bindings
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just x) f = f x

sumThree' :: String -> String -> String -> Maybe Int
sumThree' a b c = readMaybe a `bindMaybe` \l ->
                  readMaybe b `bindMaybe` \m ->
                  readMaybe c `bindMaybe` \n ->
                  Just (l+m+n)
                  
-- simplify even more using import Monad.hs
sumThree'' :: String -> String -> String -> Maybe Int
sumThree'' a b c = threeInts (readMaybe a) (readMaybe b) (readMaybe c)
