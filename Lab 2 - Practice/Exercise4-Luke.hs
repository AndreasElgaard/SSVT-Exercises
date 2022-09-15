module Exercise4 where
import Data.List
import Data.Char
import Test.QuickCheck

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

hoareTest :: (a -> Bool) -> (a -> a) -> (a -> Bool) -> [a] -> Bool
hoareTest precondition f postcondition = all (\x -> precondition x --> postcondition (f x))

x = hoareTest odd succ even [0..100]
y = hoareTest (\_ -> True) (odd) (isPermLength [1,2,3,4])


-- hoareTest isTrue isPermutation (length [1,2,3,4] == length [1,2,3,4]) [1,2,3,4]
-- Precondition: { isTrue xs } ys = quicksort xs { prop_ordered ys }
-- Postconditions: 
--   - Permutation A has same length of Permutation B
--   - Permutation A sorted is equivalent to Permutation B sorted (stronger condition)

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = False
isPermutation x [] = True
isPermutation xsPermA (x: xsPermB) 
    | x `elem` xsPermA = isPermutation xsPermA xsPermB 
    | otherwise = False

-- Assuming that the list can have duplicates, this would probably work best
isPermSort :: Ord a => [a] -> [a] -> Bool
isPermSort xs xsPerm = sort xs == sort xsPerm

-- Not really a good test
isPermLength :: Eq a => [a] -> [a] -> Bool
isPermLength xs xsPerm = length xs == length xsPerm


