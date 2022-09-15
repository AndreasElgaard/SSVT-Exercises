module Exercise4 where

import Data.Char
import Data.List
import Test.QuickCheck

-- Assuming that the list can have duplicates, this would probably work best
isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation xs xsPerm = sort xs == sort xsPerm

-- Tests
-- Checks if two lists are permutations of each other.
prop_twoEqualLists :: Bool
prop_twoEqualLists = isPermutation [1, 2, 3, 4, 5, 6] [6, 5, 4, 3, 2, 1]

-- Test Report
main :: IO Result
main = do
  putStrLn "\n=== Testing if length of the returned string is equal to 26 ===\n"
  quickCheckResult prop_twoEqualLists

-- Time Spent:
--      3 hours