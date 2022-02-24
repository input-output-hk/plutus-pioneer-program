module Main
    ( main
    ) where

import System.Environment (getArgs)
import Week06.Utils       (unsafeReadAddress, unsafeStakePubKeyHash)

main :: IO ()
main = do
    [addr'] <- getArgs
    print $ unsafeStakePubKeyHash $ unsafeReadAddress addr'
