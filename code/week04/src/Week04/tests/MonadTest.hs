-- test versions
-- the idea of this script is to createa a module 
-- that contains a monad that allows to sum three integers
module Week04.MonadTest where

threeInts :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts ma mb mc = 
    ma >>= \l ->
    mb >>= \m ->
    mc >>= \n ->
    let s = l+m+n
    in  return s    
