-- Packages Used
--  :set -package random
module Exercise1 where
-- TODO - Add comments for all the code
--      - Add explanation for why the result is as it is IE that the expected result is approx 100 above and 100 below
--      - Cleanup code
--      - Add time spent
--      - Elaborate on the maths part of the answer
import System.Random ( getStdRandom, Random(random) )
import Test.QuickCheck

-- Function provided by the assignment
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)

-- Simple List comprehension on the output of the list
-- Explanation of the function:
--      1. Run the 'probs' function with the inputted number
--      2. Return a tuple with 4 values (fourple?), each contains the occurence of the numbers specified in their respecitve filter.
--          This allows us to have an understanding of the occurence of each range of numbers 
prop_testProbs :: Int -> IO (Int, Int, Int, Int)
prop_testProbs numberOfProbs = do
    qs <- probs numberOfProbs
    return (getLengthOfFilter qs (\x -> x > 0 && x < 0.25),
        getLengthOfFilter qs (\x -> x >= 0.25 && x < 0.5),
        getLengthOfFilter qs (\x -> x >= 0.5 && x < 0.75),
        getLengthOfFilter qs (\x ->  x >= 0.75 && x < 1))

-- Helper function that returns the amount of times a number is in the range provided by the filter
getLengthOfFilter :: [a] -> (a -> Bool) -> Int
getLengthOfFilter xs filter = length ([ x | x <- xs, filter x ])

differenceQ12 :: Int -> IO Bool
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
main :: IO (Int, Int, Int, Int)
main = do
    putStrLn "\n=== Testing probs with a range of 10000===\n"
    prop_testProbs 10000

-- Observations:
--      Running 'prop_testProbs 10000' outputs similiar to the following '(2562,2509,2454,2475)'. 
--      On every re-run this function outputs similiar values that differ by roughly 80 up and down.
--      It was also observed that if 'prop_testProbs 100000' is executed a similiar pattern is observed,
--      for example '(24976,24938,25100,24986)' is an output.