
module Exercise5 where
import Data.List ( permutations, sortBy )
import Test.QuickCheck ( quickCheck, Gen, Arbitrary (arbitrary), suchThat, forAll )
import Data.Function ( on )
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
isDerangement l1 l2
  | length l1 /= length l2 = False
  | otherwise = not (any (uncurry (==)) (zip l1 l2))

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
-- Here we are testing the base case with two lists, one being deranged
prop_isCorrectDerangement :: Int -> Bool
prop_isCorrectDerangement x = isDerangement list deranned where
  deranned = getDeranHead(deran x)
  list = [0..(x-1)]

-- Here we are testing the base case with two identical lists
prop_isIncorrectDerangement :: Int -> Bool
prop_isIncorrectDerangement x = not(isDerangement list list) where
  list = [x..end]
  end = x + 10

-- Testing two lists, one of them being deranged but with an incorrect length
-- This function currently has a weakness because negative numbers dont work with it because of 
-- the fetching of the tail, currently being circumvented by a Generator.
prop_isCorrectDerangementWBadLength :: Int -> Bool
prop_isCorrectDerangementWBadLength x = not(isDerangement list (getDeranTail deranned)) where
  deranned = getDeranHead(deran x)
  list = [0..(x-1)]

-- Testing two lists, one being deranged, and we check the sum after 
prop_isCorrectDerangementAndSumChecked:: Int -> Bool
prop_isCorrectDerangementAndSumChecked x = isDerangement list deranned && (sum list == sum deranned)  where
  deranned = getDeranHead(deran x)
  list = [0..(x-1)]

-- Testing whether the function works when the inputs are flipped
prop_isSymetricalDeran :: Int -> Bool
prop_isSymetricalDeran x = isDerangement list deranned == isDerangement deranned list where
  deranned = getDeranHead(deran x)
  list = [0..(x-1)]

-- Utility functions
getDeranHead :: [[a]] -> [a]
getDeranHead [] = []
getDeranHead (x:_)= x

getDeranTail :: [a] -> [a]
getDeranTail [] = []
getDeranTail (_:xs)= xs

-- ########## Helper functions for tests ##########
-- Extra test that tests deran instead of isDeran
-- This checks that every derangement of [0..3] (deran 4) is actually a derangement.
prop_deranDerangement :: Bool
prop_deranDerangement = recursion [0 .. 3] (deran 4)


-- this one is used in the prop_deranDerangement, to go over all the combinations of deran 4
-- and check for all of them if they are a derangement.
recursion :: [Int] -> [[Int]] -> Bool
recursion _ [] = True
recursion interval (x : xs)
  | isDerangement interval x = recursion interval xs
  | otherwise = False

-- This one is True when we have found a number x in the other list. We don't use it in the end
anotherRecursion :: [Int] -> [[Int]] -> Bool
anotherRecursion _ [] = False
anotherRecursion firstLoopNumber (x : xs)
  | firstLoopNumber == x = True
  | otherwise = anotherRecursion firstLoopNumber xs

genNonZeroRange :: Gen Int
genNonZeroRange = (arbitrary :: Gen Int) `suchThat` (\x -> x > (-10) &&  (x < 10) && (x /= 0) && (x /= 1))

genNonZeroAndOneRange :: Gen Int
genNonZeroAndOneRange = (arbitrary :: Gen Int) `suchThat` (\x -> (x < 5) && (x > 1))
-- ########## Test Report ##########
main :: IO ()
main = do
  putStrLn "\n=== Testing isDerangement with Two Lists, 1 Deranged == True ===\n"
  quickCheck $ forAll genNonZeroRange prop_isCorrectDerangement
  putStrLn "\n=== Testing isDerangement wtih Two Indetical Lists == False  ===\n"
  quickCheck $ forAll genNonZeroRange prop_isIncorrectDerangement
  putStrLn "\n=== Testing isDerangement with Two Lists, 1 Deranged but with different lengths == False ===\n"
  quickCheck $ forAll genNonZeroAndOneRange prop_isCorrectDerangementWBadLength
  putStrLn "\n=== Testing isDerangement with 1 Deranged & Sum is Checked == True ===\n"
  quickCheck $ forAll genNonZeroRange prop_isCorrectDerangementAndSumChecked
  putStrLn "\n=== Testing isDerangement with 1 Deranged & Function params are flipped to check validity == True ===\n"
  quickCheck $ forAll genNonZeroRange prop_isSymetricalDeran
  putStrLn "\n=== Showing the rankings of the properties ===\n"
  putStrLn  showResults

-- Ranking of Properties
  -- Property: Two Lists, 1 Deranged & Sum is Checked (3) ,
  -- Property: Two Lists, 1 Deranged (3) ,
  -- Property: Two Lists, 1 Deranged but with different lengths (2) ,
  -- Property: Two Lists, 1 Deranged & Function params are flipped to check validity (1) ,
  -- Property: Two Indetical Lists (1) ,

-- Q: Can you automate the test process?
-- A: It is difficult to automate the testing for this function as thoroughly test this function 
--    requries a lot of resources to test due to the permutations function that embedded within it.


-- ========== LECTURE BASED FUNCTIONS ============
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p
-- ========== END OF LECTURE USED FUNCTIONS ============

-- ============ Code from the previous Exercise ========= --
arrOfProps :: [([Char], Int -> Bool)]
arrOfProps = [("Property: Two Lists, 1 Deranged", prop_isCorrectDerangement),
              ("Property: Two Indetical Lists", prop_isIncorrectDerangement),
              ("Property: Two Lists, 1 Deranged but with different lengths", prop_isCorrectDerangementWBadLength),
              ("Property: Two Lists, 1 Deranged & Sum is Checked", prop_isCorrectDerangementAndSumChecked),
              ("Property: Two Lists, 1 Deranged & Function params are flipped to check validity", prop_isSymetricalDeran)]

isEqual :: (Num a, Enum a) => (a -> Bool) -> (a -> Bool) -> Bool
isEqual leftProp rightProp = stronger [(-10)..10] leftProp rightProp && stronger [(-5)..5] rightProp leftProp

mapFunctions :: [([Char], Int)]
mapFunctions = reverse sorted where
    sorted = sortBy (compare `on` snd) rankedFunctions
    rankedFunctions = map getFunctionStrength arrOfProps

getFunctionStrength :: (a, Int -> Bool) -> (a, Int)
getFunctionStrength (functionName, leftProp) = (functionName, trueVals) where
    trueVals = length (filter (== True) arrOfBools) - 1
    arrOfBools =  map  (\(funcName, rightProp) -> stronger [(-5)..5] leftProp rightProp || isEqual leftProp rightProp) arrOfProps

showResults :: [Char]
showResults  = concatMap (\(left, right) -> left ++ " (" ++ show right ++ ") ,\n") mapFunctions
-- ============ End of Code from the previous Exercise ========= --