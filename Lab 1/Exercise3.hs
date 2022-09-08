module Exercise3 where
import Data.List
import Test.QuickCheck

genSmallRange :: Gen Int
genSmallRange = (arbitrary :: Gen Int) `suchThat` (\x -> (x<= 5) && (x>0))

genMediumRange :: Gen Int
genMediumRange = (arbitrary :: Gen Int) `suchThat` (\x -> (x<= 10) && (x>5))

genLongRange :: Gen Int
genLongRange = (arbitrary :: Gen Int) `suchThat` (\x -> (x<= 15) && (x>10))

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concatMap (insrt x) (perms xs) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

factorial:: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- i thought to go from n and multiplying n times n-1 to it. n*n-1*n-2. to dlooked for a factorial code online http://progopedia.com/example/factorial/118/ it is a basic math function
-- so i just checked for the most used amongst other haskell users. 

-- commented out as its a bit redundant 
-- exercise3:: Integer -> Integer
-- exercise3 n = factorial n

--sloow
-- TODO Explain reasoning for function
proofLengthEqualToFactorial:: Int -> Bool
proofLengthEqualToFactorial n = length(perms [1..n]) == factorial n

-- Not very useful but makes sure the function is working 
proofPermsFixed :: Bool
proofPermsFixed = perms [1..2] == [[1,2],[2,1]]

-- Test Report 
main :: IO ()
main = do
    putStrLn "\n=== Testing a base case of [1..2], not very insightful but makes sure the function works ===\n"
    quickCheckResult proofPermsFixed
    putStrLn "\n=== Testing a range of 1 to 5, takes a short amount of time to complete ===\n"
    quickCheck $ forAll genSmallRange proofLengthEqualToFactorial
    putStrLn "\n=== Testing a range of 6 to 10, takes quite a bit longer ===\n"
    quickCheck $ forAll genMediumRange proofLengthEqualToFactorial
    putStrLn "\n=== Testing a range of 11 to 15, usually takes much longer to complete ===\n"
    quickCheck $ forAll genMediumRange proofLengthEqualToFactorial

-- Still need to answer the questions:

-- Q: Is the property hard to test? If you find that it is, can you given a reason why?

-- A: 

-- Q: Give your thoughts on the following issue: when you perform the test for exercise 5,
--    what are you testing actually? Are you checking a mathematical fact? Or are you testing
--    whether perms satisfies a part of its specification? Or are you testing something else still?

-- A: 

-- Time Spent: 
