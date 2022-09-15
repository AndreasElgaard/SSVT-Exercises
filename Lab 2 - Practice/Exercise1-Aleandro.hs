-- Packages Used
--  :set -package random
module Exercise1 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)

-- This doesnt work cause of IO [Float], if it was just a [Float] it would have (probably worked)
-- checkOutput [] output =  output 
-- checkOutput (x:xs) (firstQ, secondQ, thirdQ, fouthQ) 
--     | x >= 0 && x <= 0.25 = checkOutput xs (firstQ + 1, secondQ, thirdQ, fouthQ)  
--     | x >= 0.25 && x <= 0.5 = checkOutput xs (firstQ, secondQ + 1, thirdQ, fouthQ)  
--     | x >= 0.5 && x <= 0.75 = checkOutput xs (firstQ , secondQ, thirdQ + 1, fouthQ)  
--     | x >= 0.75 && x <= 1 =  checkOutput xs (firstQ , secondQ, thirdQ, fouthQ + 1) 
--     | otherwise = error "Invalid Input"

-- Simple List comprehension on the output of the list, not sure if this is the best approach
prop_testProbs :: IO (Int, Int, Int, Int)
prop_testProbs = do
    qs <- probs 10000
    return (getLengthOfFilter qs (\x -> x >= 0 && x <= 0.25), 
        getLengthOfFilter qs (\x -> x >= 0.25 && x <= 0.5), 
        getLengthOfFilter qs (\x -> x >= 0.5 && x <= 0.75), 
        getLengthOfFilter qs (\x ->  x >= 0.75 && x <= 1))


getLengthOfFilter :: [a] -> (a -> Bool) -> Int
getLengthOfFilter xs filter = length ([ x | x <- xs, filter x ])
