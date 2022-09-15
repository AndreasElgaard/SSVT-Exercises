module Exercise2 where
import Data.List
import Test.QuickCheck



(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- My initial train of thought was: I combine both the lists with zip. with this one we get all the same index number in the same pair, once there i filter all the values to see if there is any pair with the same value. 
-- if this is true, it means it is not a derangment. however, in a derangement we will have no numbers returned by this filter, meaning it will return [] empty list. 

-- This was the result of my train of thought [] == filter (\(x,y) -> x == y) (zip l1 l2)

-- However, my linter recommended me what I wrote now. It does the same thing but cleaner. I know uncurry compares my values in the pair with the arythmetic I said, which is ==. Basically does what (\(x,y) -> x == y) did. The way I think it is now is: did you find ANY pair of numbers which are equal (uncurry). If you find them, true, so we negate it. Because we don't want any equal pairs, since this means the same number in the same index, aka, not a correct derangement.

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement l1 l2 = not (any (uncurry (==)) (zip l1 l2))

 

-- i think there should be different functions for the filter and zip function, but im confused with output types.

deran:: Int -> [[Int]]
deran n = goOver n (permutations [0..n-1])

-- I use goOver to be able to loop through the whole list of permutations I obtained in permutations [0..n-1] and check if they are a derangement with the previous function
-- I keep passing n as a parameter in goOver because I don't know a simpler way to do it, but I think there should be.

goOver:: Int -> [[Int]] ->[[Int]]
goOver  _ [] = []
goOver n (x:xs)
    | isDerangement [0..n-1] x =  x:goOver n xs
    | otherwise = goOver n xs


prop_LengthDerangement:: Eq a => [a] -> [a] -> Bool
prop_LengthDerangement l1 l2 = length l1 == length l2

--NOT FINISHEEEEED!!!!!!!!!!!!!! ERROR ON TYPE OF X !
-- if the length of our first list is the same as the second derangement, they both have the same numbers. This is because I am filtering the second list by if they have the number in the first list. If some number is not found, the second filtered list will be shorter. 

--prop_sameNumbers:: Eq a => [a] -> [a] -> Bool
--prop_sameNumbers l1 l2 = length l1 == length [x | x <- l1, anotherRecursion x l2]

-- this one is True when we have found  a number x in the other list.
anotherRecursion:: [Int] ->[[Int]] -> Bool
anotherRecursion  _ [] = False
anotherRecursion firstLoopNumber (x:xs)
    | firstLoopNumber == x =  True
    | otherwise = anotherRecursion firstLoopNumber xs

-- this checks that every derangement of [0..3] (deran 4) is actually a derangement.
prop_deranDerangement:: Bool
prop_deranDerangement = recursion [0..3] (deran 4) 

recursion:: [Int] -> [[Int]] -> Bool
recursion  _ [] = False
recursion interval (x:xs)
    | isDerangement interval x = True
    | otherwise = anotherRecursion interval xs

-- Test isDerangement with a well-chosen integer lists:
prop_isNotDerangement::Bool
prop_isNotDerangement = isDerangement [1,2,3,4] [4,1,3,2]

prop_isDerangement::Bool
prop_isDerangement = isDerangement [1,2,3,4] [4,1,2,3]

-- Possible tests for isDerangement:
-- 1: Both lists are the same length
-- 2: Both lists have the same numbers
-- 3: ?????? 

--Possible tests for deran (not in the assignment question oopsie):
-- 1: We check that [0..n-1] is the same length as all the derangements.
-- 2: We check the derangements obtained are part of the whole permutations list
-- 3: We check for each index there is no derangement group that have the same number as the [0..n-1] list.



stronger :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)

weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
weaker   xs p q = stronger xs q p

getFunctionStrength :: (a, Int -> Bool) -> (a, Int)
getFunctionStrength (functionName, leftProp) = (functionName, trueVals) where
    -- Since we are checking if it is stronger than itself we reduce one point
    trueVals = length (filter (== True) arrOfBools) - 1 
    -- Check if the left prop is stronger to or equal to the right prop
    arrOfBools =  map  (\(funcName, rightProp) -> (stronger [(-10)..10] leftProp rightProp) || isEqual leftProp rightProp) arrOfProps

isEqual :: (Num a, Enum a) => (a -> Bool) -> (a -> Bool) -> Bool
isEqual leftProp rightProp = (stronger JO[(-10)..10] leftProp rightProp) && (stronger [(-10)..10] rightProp leftProp)

-- my prop return bool but you use Int -> Bool for your algorithm. I don't know whether to change your algorithm or you put the integer 
arrOfProps :: [([Char], Bool)]
arrOfProps = [("Prop One", prop_deranDerangement), ("Prop Two", prop_isDerangement)]




