module Exercise3 where
import Test.QuickCheck
    ( suchThat, forAll, quickCheck, Arbitrary(arbitrary), Gen )

-- The Generators have been given ranges that portray how long the tests will take
-- this has been done for ease of testing and showing the issue with exponential numbers
genSmallRange :: Gen Int
genSmallRange = (arbitrary :: Gen Int) `suchThat` (\x -> (x<= 5) && (x>0))

genSmallNegRange :: Gen Int
genSmallNegRange = (arbitrary :: Gen Int) `suchThat` (\x -> (x < 0) && (x > (-5) ))

genMediumRange :: Gen Int
genMediumRange = (arbitrary :: Gen Int) `suchThat` (\x -> (x<= 10) && (x>5))

genMediumNegRange :: Gen Int
genMediumNegRange = (arbitrary :: Gen Int) `suchThat` (\x -> (x < (-10)) && (x > (-5)))

genLongRange :: Gen Int
genLongRange = (arbitrary :: Gen Int) `suchThat` (\x -> (x<= 15) && (x>10))

genLongNegRange :: Gen Int
genLongNegRange = (arbitrary :: Gen Int) `suchThat` (\x -> (x < (-10)) && (x > (-15)))

-- Provided code
--  This function recursively opens a map up until all permutations of the 
--  map are exploded 
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concatMap (insrt x) (perms xs) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

-- Returns the factorial of the number provided,  Starting from n and multing n times n-1
-- http://progopedia.com/example/factorial/118/ was used for this function
factorial:: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)


-- Testing that the length of the permutations of the provided list is equal to the factorial
-- of the inputted number
prop_LengthEqualToFactorial:: Int -> Bool
prop_LengthEqualToFactorial n = length(perms [1..n]) == factorial n

-- Assures the function is working as intended as a base case
prop_PermsFixed :: Bool
prop_PermsFixed = perms [1..2] == [[1,2],[2,1]]

-- Test Report 
main :: IO ()
main = do
    putStrLn "\n=== Testing a base case of [1..2], not very insightful but makes sure the function works ===\n"
    quickCheck prop_PermsFixed
    putStrLn "\n===!! Positive Number Testing !!===\n"
    putStrLn "\n=== Testing a range of 1 to 5, takes a short amount of time to complete ===\n"
    quickCheck $ forAll genSmallRange prop_LengthEqualToFactorial
    putStrLn "\n=== Testing a range of 6 to 10, takes quite a bit longer ===\n"
    quickCheck $ forAll genMediumRange prop_LengthEqualToFactorial
    putStrLn "\n=== Testing a range of 11 to 15, usually takes much longer to complete ===\n"
    quickCheck $ forAll genMediumRange prop_LengthEqualToFactorial
    putStrLn "\n===!! Negative Number Testing !!===\n"
    putStrLn "\n=== Testing a range of -1 to -5, takes a short amount of time to complete ===\n"
    quickCheck $ forAll genSmallRange prop_LengthEqualToFactorial
    putStrLn "\n=== Testing a range of -6 to -10, takes quite a bit longer ===\n"
    quickCheck $ forAll genMediumRange prop_LengthEqualToFactorial
    putStrLn "\n=== Testing a range of -11 to -15, usually takes much longer to complete ===\n"
    quickCheck $ forAll genMediumRange prop_LengthEqualToFactorial

-- Q: Is the property hard to test? If you find that it is, can you given a reason why?

-- A: Similiarly to the Exercise2, this property is difficult to test with a large set of numbers
--    due to the exponential size of the computation required. The mathical reasoning behind this is the 
--    that the length of the array of permuations is the factorial of the number of values in the array. 

-- Q: Give your thoughts on the following issue: when you perform the test for exercise 5,
--    what are you testing actually? Are you checking a mathematical fact? Or are you testing
--    whether perms satisfies a part of its specification? Or are you testing something else still?

-- A: We are testing that perms is satisfying part of its specification because we are providing perms with 
--    a small range of values on which to test. We are not testing or proving the mathemetical fact. To portray the
--    difficulty in testing we provided multiple ranges of numbers to show the growing computation time to test.

-- Time Spent: 1.5 hours, mainly disucssing how to best test it  
