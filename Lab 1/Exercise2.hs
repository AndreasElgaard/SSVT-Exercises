module Exercise2 where
import Test.QuickCheck
import Data.List ( subsequences )

genSmallRange :: Gen Integer
genSmallRange = (arbitrary :: Gen Integer) `suchThat` (\x -> (x<= 10) && (x>0))

-- Creates a generator which provides a range from 1 to 25, this is done due to the time required for larger numbers
-- 25 is used as a maximum range as anything larger could take substancially longer to test
genMediumRange :: Gen Integer
genMediumRange = (arbitrary :: Gen Integer) `suchThat` (\x -> (x<= 25) && (x>10))

genLongRange :: Gen Integer
genLongRange = (arbitrary :: Gen Integer) `suchThat` (\x -> (x<= 35) && (x>25))

-- Assuming the property [1..n], only positive numbers are being calcualated
finiteSet :: Integer -> Int
finiteSet x
    | x > 0  = length (subsequences [1..x])
    | otherwise = 0

-- Makes sure the function works as expected, not too useful but good to have
proofEquality :: Bool
proofEquality = 2^10 == finiteSet 10

-- Tests the function with a limited range, if a large range of number is used 
-- test will hang for a long time
-- !IMP Please use the genSmallRange Generator
proofEqualityRange :: Integer -> Bool
proofEqualityRange x = 2^x == finiteSet x

-- Questions:
-- Q: Is the property hard to test? If you find that it is, can you given a reason why?

-- A: The said property is hard to test due to the property explicitly stating that length
--    of PowerSets rises exponentially (2^n), hence the problem is not tractible
--    in Linear/Quadratic time. THe problem can be tested within a small range of values
--    for example denoted by the generator specified as 'genSmallRange', however
--    when this range of values grow, the time it takes to run the tests rises exponentially. 


-- Q: Give your thoughts on the following issue: when you perform
--    the test for exercise 4, what are you testing actually? Are you
--    checking a mathematical fact? Or are you testing whether subsequences
--    satisfies a part of its specification? Or are you testing something else still?

-- A: We are simply testing the property proven by induction on example 4, on a
--    small range of values. The test itself only confirms validity of property on 
--    the defined limited range of values, rather than proving the mathemetical foundation.
--    For the range specified, the tests confirm that subsequences' length conforms 
--    to what the property postulates.

-- Test Report 
main :: IO ()
main = do
    putStrLn "\n=== Testing a base case of 10, not very insightful but makes sure the function works ===\n"
    quickCheckResult proofEquality
    putStrLn "\n=== Testing a range of 1 to 10, takes a short amount of time to complete ===\n"
    quickCheck $ forAll genSmallRange proofEqualityRange
    putStrLn "\n=== Testing a range of 11 to 25, takes a bit longer amount of time ===\n"
    quickCheck $ forAll genMediumRange proofEqualityRange
    putStrLn "\n=== Testing a range of 26 to 35, usually takes much longer to complete ===\n"
    quickCheck $ forAll genMediumRange proofEqualityRange


-- Indiction of Time Spent: 50 minutes -> mainly took long as we over complocated it in the begiining + haskell beginner confusion
--TODO State why negative numbers dont work with this function.. there is a mathematical reason



