module Exercise2 where
import Test.QuickCheck
import Data.List ( subsequences )

-- The Generators have been given ranges that portray how long the tests will take
-- this has been done for ease of testing and showing the issue with exponential numbers
genSmallRange :: Gen Integer
genSmallRange = (arbitrary :: Gen Integer) `suchThat` (\x -> (x<= 10) && (x>0))

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
prop_TestEquality :: Bool
prop_TestEquality = 2^10 == finiteSet 10

-- Tests the function with a limited range, if a large range of number is used 
-- test will hang for a long time
-- !IMP Please use the genSmallRange Generator
prop_EqualityRange :: Integer -> Bool
prop_EqualityRange x = 2^x == finiteSet x

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
    quickCheckResult prop_TestEquality
    putStrLn "\n=== Testing a range of 1 to 10, takes a short amount of time to complete ===\n"
    quickCheck $ forAll genSmallRange prop_EqualityRange
    putStrLn "\n=== Testing a range of 11 to 25, takes a bit longer amount of time ===\n"
    quickCheck $ forAll genMediumRange prop_EqualityRange
    putStrLn "\n=== Testing a range of 26 to 35, usually takes much longer to complete ===\n"
    quickCheck $ forAll genMediumRange prop_EqualityRange
    -- putStrLn "\n=== Testing a range of negative numbers, showing the function only works with natural numbers ===\n"
    -- quickCheck prop_NegativeNumbersDontWork


-- Indiction of Time Spent: 50 minutes -> mainly took long as we over complocated it in the beginning + haskell beginner confusion

-- Note: tests only consider numbers greater then 0. Negative numbers are not allowed.
-- Reason being: Permuations for n numbers = n!
-- But, factorials of negative integers are NOT defined.
-- Contradiction: 0 . (0 - 1) ! = 0!, but 0! = 1, hence 0 != 0!
-- Ref. to: https://math.stackexchange.com/questions/927382/what-does-the-factorial-of-a-negative-number-signify


-- Extra Test Concepts
-- prop_NegativeNumbersDontWork :: Integer -> Property
-- prop_NegativeNumbersDontWork n = n < 0 && n > (-20) ==> 2^n /= finiteSet n
--  To test the above negative number rule a test was created to show that 2^ of a negative
--  number will raise a "Negative Exponent" excpetion, didnt get it working proplery with Monads though. 
 