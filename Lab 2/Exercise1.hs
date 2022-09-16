-- Packages Used
--  :set -package random
module Exercise1 where

-- Time spent: 4 h (including understanding the IO monad)

-- TODO - DONE. Add comments for all the code
--      - REVISE. Add explanation for why the result is as it is IE that the expected result is approx 100 above and 100 below
--      - REVISE. Cleanup code
--      - NOT SURE. Elaborate on the maths part of the answer
import System.Random ( getStdRandom, Random(random) )
import Test.QuickCheck
import GhcPlugins (nameEnvElts)

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
--          This allows us to have an understanding of the occurence of each range of numbers (the ranges are the quartiles specified in the question assignment) 
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


-- Observations:
--      Running 'prop_testProbs 10000' outputs similiar to the following '(2562,2509,2454,2475)'. 
--      On every re-run this function outputs similiar values that differ by roughly 80 up and down.
--      It was also observed that if 'prop_testProbs 100000' is executed a similiar pattern is observed,
--      for example '(24976,24938,25100,24986)' is an output.
-- 
--      Initial testing: To test the previous code, we created 3 tests which substract the ocurrence of two different quartiles
--      The sustraction is the difference they have between each other. Because of the previous observation, we
--      qualify as true one the difference is smaller than n `div` 100 (which we will modify later)
--      The value n smaller than 100 is hard to give it a approximate valid substraction since the
--      value is too small.

prop_testDifferenceQ12 :: Int -> IO Bool
prop_testDifferenceQ12 n = do
    qs <- probs n
    return (n `div` 100 >= getLengthOfFilter qs (\x -> x > 0 && x < 0.25) - getLengthOfFilter qs (\x -> x >= 0.25 && x < 0.5))


-- tests for difference quartile 2 and 3
prop_testDifferenceQ23 :: Int -> IO (Bool)
prop_testDifferenceQ23 n = do
    qs <- probs n
    return (n `div` 100 >= getLengthOfFilter qs (\x -> x >= 0.25 && x < 0.5) - getLengthOfFilter qs (\x -> x >= 0.5 && x < 0.75))

-- tests for difference quartile 3 and 4
prop_testDifferenceQ34 :: Int -> IO (Bool)
prop_testDifferenceQ34 n = do
    qs <- probs n
    return (n `div` 100 >= getLengthOfFilter qs (\x -> x >= 0.5 && x < 0.75) - getLengthOfFilter qs (\x -> x >= 0.75 && x < 1))

-- As we see in the previous tests, having the value n `div` 100 could work. However if a probs gave wrong outputs,
-- like '(4000,4000,1000,1000)', the test difference between quartile 1 and 2 would still be true.
-- To fix this we could do
--  CASE 1: A testing function where all the differences should be True
--  CASE 2: Instead of substracting, get the ocurrence number of one quartile and use n `div` 100 as a 
--     bound (upperbound and lowerbound) for the number n/4 which is the rough number whch should 
--     follow as the problem question states.

-- This is the test for evaluationg the validity of one quartile. Changing the filter would allow us to see the other quartiles.

prop_case2:: Int -> IO (Bool)
prop_case2 n = do
    qs <- probs n 
    return (n `div` 4 + n `div` 100 >= getLengthOfFilter qs (\x -> x > 0 && x < 0.25) && n `div` 4 - n `div` 100 <= getLengthOfFilter qs (\x -> x > 0 && x < 0.25) )


main :: IO ()
main = do
  --For Testing the behaviour of probs function with n = 10000 
  -- prop_testProbs 10000
  --For Testing the difference between quartile 1 and 2 are following the expected behaviour
  -- prop_testDifferenceQ12 10000
  --For  Testing the difference between quartile 2 and 3 are following the expected behaviour ===\n"
  --prop_testDifferenceQ23 10000
  --For "\n===  Testing the difference between quartile 3 and 4 are following the expected behaviour ===\n"
  --prop_testDifferenceQ34 10000
  --For "\n=== Testing the behaviour of probs function with n = 100000 ===\n"
  --prop_testProbs 100000
  --For "\n=== Testing only one quartile wihtout using difference ===\n"
  -- prop_case2 10000
  