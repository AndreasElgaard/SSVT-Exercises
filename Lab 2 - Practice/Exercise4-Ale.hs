module Exercise4 where
import Data.List
import Data.Char
-- import System.Random
import Test.QuickCheck


isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation = isPermutation

-- Assuming that the list can have duplicates, this would probably work best
isPermSort :: Ord a => [a] -> [a] -> Bool
isPermSort xs xsPerm = sort xs == sort xsPerm

-- Not really a good test
isPermLength :: Ord a => [a] -> [a] -> Bool
isPermLength xs xsPerm = length xs == length xsPerm

-- If the list does not have duplicates: (This fails with duplicates)
isPermElems :: Eq a => [a] -> [a] -> Bool
isPermElems [] [] = False
isPermElems x [] = True
isPermElems xs (x: xsPerm) 
    | x `elem` xs = isPermElems xs xsPerm
    | otherwise = False