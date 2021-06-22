module Week08.QuickCheck where

prop_simple :: Bool
prop_simple = 2 + 2 == (4 :: Int)

-- Insertion sort code:

-- | Sort a list of integers in ascending order.
--
-- >>> sort [5,1,9]
-- [1,5,9]
--
sort :: [Int] -> [Int] -- not correct
sort []     =  []
sort (x:xs) =  insert x $ sort xs

-- | Insert an integer at the right position into an /ascendingly sorted/
-- list of integers.
--
-- >>> insert 5 [1,9]
-- [1,5,9]
--
insert :: Int -> [Int] -> [Int] -- not correct
insert x []                     =  [x]
insert x (y:ys)  | x <= y       =  x : y : ys
                 | otherwise    =  y : insert x ys

isSorted :: [Int] -> Bool
isSorted []           = True
isSorted [_]          = True
isSorted (x : y : ys) = x <= y && isSorted (y : ys)

prop_sort_sorts :: [Int] -> Bool
prop_sort_sorts xs = isSorted $ sort xs

prop_sort_preserves_length :: [Int] -> Bool
prop_sort_preserves_length xs = length (sort xs) == length xs
