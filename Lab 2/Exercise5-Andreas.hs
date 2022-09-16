module Exercise5 where

import Data.List
import Test.QuickCheck

-- ########## Explanation and thought process ##########
-- My initial train of thought was: I combine both the lists with zip.
-- With this one we get all the same index number in the same pair, once there I filter all the values to see if there is any pair with the same value.
-- If this is true, it means it is not a derangment. However, in a derangement we will have no numbers returned by this filter, meaning it will return [] empty list.
-- This was the result of my train of thought [] == filter (\(x,y) -> x == y) (zip l1 l2)
-- However, my linter recommended me what I wrote now. It does the same thing but cleaner.
-- I know uncurry compares my values in the pair with the arythmetic I said, which is ==. Basically does what (\(x,y) -> x == y) did.
-- The way I think it is now is: did you find ANY pair of numbers which are equal (uncurry). If you find them, true, so we negate it.
-- Because we don't want any equal pairs, since this means the same number in the same index, aka, not a correct derangement.

-- Function that checks if one list is a derangement of another one.
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement l1 l2 = not (any (uncurry (==)) (zip l1 l2))

-- Function that generates a list of all derangements of the list [0..n-1].
deran :: Int -> [[Int]]
deran n = goOver n (permutations [0 .. n -1])

-- I use goOver to be able to loop through the whole list of permutations I obtained in permutations [0..n-1] and check if they are a derangement with the previous function
-- I keep passing n as a parameter in goOver because I don't know a simpler way to do it, but I think there should be.
goOver :: Int -> [[Int]] -> [[Int]]
goOver _ [] = []
goOver n (x : xs)
  | isDerangement [0 .. n -1] x = x : goOver n xs
  | otherwise = goOver n xs

-- ########## Tests ##########
-- Checks to see if the relation is symmetric
prop_isSymmetrical :: Eq a => [a] -> [a] -> Bool
prop_isSymmetrical l1 l2 = isDerangement l1 l2 == isDerangement l2 l1 

-- sort both lists and compare them. if equal, it has the same numbers. if false, something is missing or extra.
prop_sameNumbers :: [Int] -> [Int] -> Bool
prop_sameNumbers l1 l2 = sort l1 == sort l2

-- This checks that every derangement of [0..3] (deran 4) is actually a derangement.
prop_deranDerangement :: Bool
prop_deranDerangement = recursion [0 .. 3] (deran 4)

-- Test isDerangement within a well-chosen integer lists return false
prop_isNotDerangement :: Bool
prop_isNotDerangement = not (isDerangement [1, 2, 3, 4] [1, 3, 2, 4])

-- Test isDerangement within a well-chosen integer lists returns true
prop_isDerangement :: Bool
prop_isDerangement = isDerangement [1, 2, 3, 4] [4, 1, 2, 3]

-- ########## Helper functions for tests ##########

-- this one is used in the prop_deranDerangement, to go over all the combinations of deran 4
recursion :: [Int] -> [[Int]] -> Bool
recursion _ [] = False
recursion interval (x : xs)
  | isDerangement interval x = True
  | otherwise = recursion interval xs

-- This one is True when we have found a number x in the other list. We don't use it in the end
anotherRecursion :: [Int] -> [[Int]] -> Bool
anotherRecursion _ [] = False
anotherRecursion firstLoopNumber (x : xs)
  | firstLoopNumber == x = True
  | otherwise = anotherRecursion firstLoopNumber xs


-- ########## Test Report ##########
main :: IO Result
main = do
  putStrLn "\n=== Testing if  ===\n"
  -- quickCheck prop_LengthDerangement

  putStrLn "\n=== Testing if ===\n"
  quickCheckResult prop_deranDerangement

  putStrLn "\n=== Testing if  ===\n"
  quickCheckResult prop_isNotDerangement

  putStrLn "\n=== Testing if ===\n"
  quickCheckResult prop_isDerangement

-- Q: Can you automate the test process?
-- A:

-- getFunctionStrength :: (a, Int -> Bool) -> (a, Int)
-- getFunctionStrength (functionName, leftProp) = (functionName, trueVals)
--   where
--     trueVals = length (filter (== True) arrOfBools) - 1 -- Since we are checking if it is stronger than itself we reduce one point
--     arrOfBools = map (\(funcName, rightProp) -> (stronger [(-10) .. 10] leftProp rightProp) || isEqual leftProp rightProp) arrOfProps -- Check if the left prop is stronger to or equal to the right prop

-- isEqual :: (Num a, Enum a) => (a -> Bool) -> (a -> Bool) -> Bool
-- isEqual leftProp rightProp = (stronger JO [(-10) .. 10] leftProp rightProp) && (stronger [(-10) .. 10] rightProp leftProp)

-- my prop return bool but you use Int -> Bool for your algorithm. I don't know whether to change your algorithm or you put the integer

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all



arrOfProps :: [([Char], Bool)]
arrOfProps = [("Prop One", prop_deranDerangement), ("Prop Two", prop_isDerangement)]

stronger :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\x -> p x --> q x)

weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
weaker xs p q = stronger xs q p

-- Possible tests for isDerangement:
-- 1: Both lists are the same length
-- 2: Both lists have the same numbers
-- 3: If they are symmetrical (This test does not work in the hs file tho. Maybe its not a good test?)