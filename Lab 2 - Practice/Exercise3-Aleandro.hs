module Exercise3 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Data.Function

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- The stronger function doesnt work for me yet.. forall isnt found and i cant find which package its imported from
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p


-- Each proposition was split into the left and right side to be able to test their strength / weakness independantly. 
prop_oneL :: Int -> Bool
prop_oneL x =  even x && x > 3

prop_oneR :: Int -> Bool
prop_oneR = even

prop_twoL :: Int -> Bool
prop_twoL x = even x || x > 3

prop_twoR :: Int -> Bool
prop_twoR = even

prop_threeL :: Int -> Bool
prop_threeL x = (even x && x > 3)|| even x

prop_threeR :: Int -> Bool
prop_threeR = even

prop_fourL :: Int -> Bool
prop_fourL = even

prop_fourR :: Int -> Bool
prop_fourR x =  (even x && x > 3) || even x

prop_even :: Int -> Bool
prop_even = even

-- Store all functions in a tuple with a key, this key is used to know which function is being processes
-- All of the functions which are the same (use "even") are abstracted to 'Prop Even' 
arrOfProps :: [([Char], Int -> Bool)]
arrOfProps = [("Prop One (L) -> (even x && x > 3)", prop_oneL),
              ("Prop Two (L) -> (even x || x > 3) ", prop_twoL),
              ("Prop Three (L) -> ((even x && x > 3) || even x)", prop_threeL), 
              ("Prop Four (R) -> (even x && x > 3) || even x)", prop_fourR), 
              ("Prop Even -> Even", prop_even)]

-- Iterate over each function and check all the permutations which it is stronger or equal to 
--  Output a tuple with the key of the function and the number of 
-- Explanation of function attributes:
--      functionName ==>  Value of the left hand side in the arrOfProps tuples, example -> "Prop One L"
--      leftProp     ==>  Refers to the left hand side of the prop to be tested. for example prop_oneL,
--                          this value is used to check the strenght of a right hand side value.
-- How we determine strength:
--      1. If the passed leftProp attribute is stronger or equal to the right hand side, it is returned as True
--      2. All the Trues for each permutation is counted, and the value of this is the strength of the prop
getFunctionStrength :: (a, Int -> Bool) -> (a, Int)
getFunctionStrength (functionName, leftProp) = (functionName, trueVals) where
    -- Since we are checking if leftProp is stronger than itself we reduce one point
    -- Explanation: when evaluating leftProp with all the other props it is also evaluating itself, which is always equal,
    -- Therefore the strenght of each props is all the 'True' values from arrOfBools - 1
    trueVals = length (filter (== True) arrOfBools) - 1 
    -- Check if the left prop is stronger to or equal to the right prop
    arrOfBools =  map  (\(funcName, rightProp) -> (stronger [(-10)..10] leftProp rightProp) || isEqual leftProp rightProp) arrOfProps


-- Checks if comparing the left to the right and the right to the left both equates to True
-- If output == True, the left is equal to the right
isEqual :: (Num a, Enum a) => (a -> Bool) -> (a -> Bool) -> Bool
isEqual leftProp rightProp = (stronger [(-10)..10] leftProp rightProp) && (stronger [(-10)..10] rightProp leftProp)

-- Description of the Function
-- 1. Iterate over 'arrOfProps', apply getFunctionStrength to each iteration
-- 2. Sorts the list by the second value of each tuple, is ascending by default
-- 3. Reverses the list to become descending order
mapFunctions :: [([Char], Int)]
mapFunctions = reverse sorted where
    sorted = sortBy (compare `on` snd) rankedFunctions
    rankedFunctions = map (\functionTuple -> getFunctionStrength functionTuple) arrOfProps

-- Explanation of the props;
-- TODO Add explanations
prop_testOne :: Bool
prop_testOne = stronger [(-10)..10] prop_oneL prop_oneR == True

prop_testTwo :: Bool
prop_testTwo = stronger [(-10)..10] prop_twoL prop_twoR == False

prop_testThree :: Bool
prop_testThree = stronger [(-10)..10] prop_threeL prop_threeR == True && stronger [(-10)..10] prop_threeR prop_threeL == True

prop_testFour :: Bool
prop_testFour = stronger [(-10)..10] prop_fourL prop_fourR == True && stronger [(-10)..10] prop_fourR prop_fourL == True