-- Packages Used
--  :set -package random
module Exercise1 where
import Data.List
import Data.Char
import Test.QuickCheck
import System.Random


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
    return (getLengthOfFilter qs (\x -> x > 0 && x < 0.25), 
        getLengthOfFilter qs (\x -> x >= 0.25 && x < 0.5), 
        getLengthOfFilter qs (\x -> x >= 0.5 && x < 0.75), 
        getLengthOfFilter qs (\x ->  x >= 0.75 && x < 1))


getLengthOfFilter :: [a] -> (a -> Bool) -> Int
getLengthOfFilter xs filter = length ([ x | x <- xs, filter x ])


prop_testDifferenceQ12 :: Int -> Property
prop_testDifferenceQ12 n = n > 1000 ==> differenceQ12 n ==  True

differenceQ12 :: Int -> IO (Bool)
differenceQ12 n = do
    qs <- probs n
    return (100 >= getLengthOfFilter qs (\x -> x > 0 && x < 0.25) - getLengthOfFilter qs (\x -> x >= 0.25 && x < 0.5))
 
         
-- 100 <= (\x -> head x - x !! 1) prop_testProbs

-- tests for the other quartiles
prop_testDifferenceQ23 :: Int -> IO (Bool)
prop_testDifferenceQ23 n = do
    qs <- probs n
    return (100 >= getLengthOfFilter qs (\x -> x >= 0.25 && x < 0.5) - getLengthOfFilter qs (\x -> x >= 0.5 && x < 0.75))

prop_testDifferenceQ34 :: Int -> IO (Bool)
prop_testDifferenceQ34 n = do
    qs <- probs n
    return (100 >= getLengthOfFilter qs (\x -> x >= 0.5 && x < 0.75) - getLengthOfFilter qs (\x -> x >= 0.75 && x < 1))
       

-- Test Report 
main :: IO ()
main = do
    putStrLn "\n=== Testing quartile 1 and 2 if their difference is higher than 100 ===\n"
    --quickCheck prop_testDifferenceQ12
    putStrLn "\n=== Testing quartile 2 and 3 if their difference is higher than 100 ===\n"
    --quickCheck prop_testDifferenceQ23
    putStrLn "\n=== Testing quartile 3 and 4 if their difference is higher than 100 ===\n"
    --quickCheck prop_testDifferenceQ34

