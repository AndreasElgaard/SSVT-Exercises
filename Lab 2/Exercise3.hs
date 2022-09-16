module Exercise3 where
import Data.List ( sortBy )
import Data.Function ( on )
import System.Posix.Internals (puts)

-- TIME SPENT: Approx 4 hours

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

-- Each proposition was split into the left and right side 
-- to be able to test their strength / weakness independently. 
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

-- Store all functions in a tuple with a key,
-- this key is used to know which function is being processed.
-- All of the functions which are the same (use "even") are abstracted to 'Prop Even' 
arrOfProps :: [([Char], Int -> Bool)]
arrOfProps = [("Prop One (L) -> (even x && x > 3)", prop_oneL),
              ("Prop Two (L) -> (even x || x > 3) ", prop_twoL),
              ("Prop Three (L) -> ((even x && x > 3) || even x)", prop_threeL),
              ("Prop Four (R) -> (even x && x > 3) || even x)", prop_fourR),
              ("Prop Even -> Even", prop_even)]

-- Iterate over each function and check all the permutations which it is stronger or equal to.
-- Output a tuple with the key of the function and the number 
-- of properties it is stronger/equal to (strength).
-- Explanation of function attributes:
--      functionName ==>  Value of the left hand side in the arrOfProps tuples, example -> "Prop One L"
--      leftProp     ==>  Refers to the left hand side of the prop to be tested. for example prop_oneL,
--                          this value is used to check the strength of a right hand side value.
-- How we determine strength:
--      1. If the passed leftProp attribute is stronger or equal to the right hand side,
--         it is returned as True.
--      2. All the Trues for each permutation is counted, 
--         and the value of this is the strength of the prop.

getFunctionStrength :: (a, Int -> Bool) -> (a, Int)
getFunctionStrength (functionName, leftProp) = (functionName, trueVals) where
    -- Since we are checking if leftProp is stronger than itself we reduce one point
    -- Explanation: when evaluating leftProp with all the other props it is also evaluating itself, which is always equal,
    -- Therefore the strength of each props is all the 'True' values from arrOfBools - 1
    trueVals = length (filter (== True) arrOfBools) - 1
    -- Check if the left prop is stronger to or equal to the right prop
    arrOfBools =  map  (\(funcName, rightProp) -> stronger [(-10)..10] leftProp rightProp || isEqual leftProp rightProp) arrOfProps


-- Checks if comparing the left to the right and the right to the left both equates to True
-- If output == True, the left is equal to the right
isEqual :: (Num a, Enum a) => (a -> Bool) -> (a -> Bool) -> Bool
isEqual leftProp rightProp = stronger [(-10)..10] leftProp rightProp && stronger [(-10)..10] rightProp leftProp

-- Description of the Function
-- 1. Iterate over 'arrOfProps', apply getFunctionStrength to each iteration
-- 2. Sorts the list by the second value of each tuple, is ascending by default
-- 3. Reverses the list to become descending order
mapFunctions :: [([Char], Int)]
mapFunctions = reverse sorted where
    sorted = sortBy (compare `on` snd) rankedFunctions
    rankedFunctions = map (\functionTuple -> getFunctionStrength functionTuple) arrOfProps

-- Explanation of the props;
prop_testOne :: Bool
prop_testOne = stronger [(-10)..10] prop_oneL prop_oneR

prop_testTwo :: Bool
prop_testTwo = not (stronger [(-10)..10] prop_twoL prop_twoR)

-- verifying that LHS and RHS of property comparison returns equivalent strength properties
-- i.e. both have equal strength.
prop_testThree :: Bool
prop_testThree = stronger [(-10)..10] prop_threeL prop_threeR && stronger [(-10)..10] prop_threeR prop_threeL

-- verifying that LHS and RHS of property comparison returns equivalent strength properties
-- i.e. both have equal strength.
prop_testFour :: Bool
prop_testFour = stronger [(-10)..10] prop_fourL prop_fourR  && stronger [(-10)..10] prop_fourR prop_fourL

-- Helper function created to show the rankings
showResults :: [Char]
showResults  = concatMap (\(left, right) -> left ++ " (" ++ show right ++ ") \n") mapFunctions

-- Test Report
main :: IO ()
main = do
    putStrLn "\n=== Is  (even x && x > 3) stronger than even? ===\n"
    print  prop_testOne
    putStrLn "\n=== Is  (even x || x > 3) stronger than even? ===\n"
    print  prop_testTwo
    putStrLn "\n=== Is  ((even x && x > 3) || even x) stronger equal to even? ===\n"
    print  prop_testThree
    putStrLn "\n=== Is  even stronger or equal to ((even x && x > 3) || even x)? ===\n"
    print prop_testFour
    putStrLn "\n=== Ranking the strongest properties ===\n"
    putStrLn showResults

-- Rankings of the props by strength (or equality): 
-- NOTE: 3 indeed showing the same cardinality of strength due to the even x property being repeated
--       in all 4 examples.
--      Shows the prop on the left, number of times it was stronger
--      [("Prop One (L) -> (even x && x > 3)", 4),
--      ("Prop Even -> Even", 3),
--      ("Prop Four (R) -> (even x && x > 3) || even x)", 3),
--      ("Prop Three (L) -> ((even x && x > 3) || even x)", 3),
--      ("Prop Two (L) -> (even x || x > 3) ",0)]

-- •(\ x -> even x && x > 3) or even:
--      LHS deals with even numbers greater than 3, RHS deals with all even numbers, hence LHS
--      is a subset of RHS (i.e. LHS is stonger)
-- •(\ x -> even x || x > 3) or even
--      LHS deals with even numbers greater, or numbers greater than 3, RHS deals with all even numbers,
--      hence RHS is a subset of LHS since from 3 onwards, all numbers are valid (i.e. RHS is stonger).
-- •(\ x -> (even x && x > 3) || even x) or even
--      LHS deals with even numbers greater then 3, or even numbers RHS deals with all even numbers,
--      hence RHS and LHS are of the same set (i.e. RHS is as strong as LHS).
-- •even or (\ x -> (even x && x > 3) || even x)
--      RHS deals with even numbers greater then 3, or even numbers LHS deals with all even numbers,
--      hence LHS and RHS are of the same set (i.e. LHS is as strong as RHS).

