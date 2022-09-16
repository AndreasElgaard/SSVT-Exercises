module Exercise4 where
import Data.List ( permutations, sort, sortBy )
import Test.QuickCheck ( quickCheck )
import System.Random ( getStdRandom, Random(randomR) )
import qualified Control.Monad.IO.Class
import Data.Function ( on )

-- Time Spent: 4 hours

-- Explanation and thought process
--     1. First create the function to assure two arrays are a permutation of each other
--     2. Create props that are relavent to the problem which can take an input for testing
--     3. Attempt to make an automated testing function
--     4. Compare the props from (2.) using the stronger or weaker functions

isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation  = isPermSort

-- To check is two lists are permutations of each other they can be sorted and compared
--  this makes sure that the length of the lists are equal and the content within is the same 
isPermSort :: Ord a => [a] -> [a] -> Bool
isPermSort [] [] = True
isPermSort xs xsPerm = sort xs == sort xsPerm

-- =============== PROPS ============

-- Testing that the same arrays is a permutation of itself
-- This is the base case
prop_baseCaseTrue :: Int -> Bool
prop_baseCaseTrue start  =  isPermutation x x where
    x = [start..end]
    end = start+10

-- Testing that two separate arrays with the same length and different
--  content are not permutations of each other
prop_baseCaseDifferent :: Int -> Bool
prop_baseCaseDifferent start =  not (isPermutation l1 l2) where
    l1 = [start..end]
    l2 = [(start+10)..(end+10)]
    end = start+10

-- Testing that a permutation of a generated array is indeed a permutation of itself
--  IMPROVEMENTS: It would be prefereable that a random permutation is selected, but 
--                I coudnt get that to work because of typing
prop_validPerm :: Int -> Bool
prop_validPerm start = isPermutation [start..end] headOfPerms where
    (headOfPerms:_) = permutations [start..end]
    end = start+10

-- Testing that two arrays of similiar content but -1 length to each other are not infact
--  permutations of each other
prop_testDiffGeneratedLists :: Int -> Bool
prop_testDiffGeneratedLists start = not (isPermutation l1 l2) where
    l1 = [start..end]
    l2 = [start..end-1]
    end = start+10

-- Automatically generating two random Int Lists and checking that the two arrays 
--  are not permutations of each other (not perfect)
prop_testRandomGeneratedPerms :: Int -> IO ()
prop_testRandomGeneratedPerms n = testRandomGenList 1 n isPermutation False
-- =============== END OF PROPS  ================

-- =============== CREATING  AUTOMATED TESTING =================
-- FLAW WITH THIS FUNCTION
-- The code is assuming that the two generated random lists are not permutations of each other
--  This is not a valid assumption so sometimes the tests will fail because the random arrays are permutations of each other
-- Ways around this are to find a way to tell genTwoIntList to filter , but that would need to call the isPermutation function... which is what we are testing
-- What this function does:
--      Runs a function 'fCounter' times and checks that the expected output is always correct
-- Improvements to the function:
--      Instead of expectedOut it should be a method of evaluation.. but i havent gotten there yet
testRandomGenList :: Int -> Int -> ([Int] -> [Int] -> Bool)
                    -> Bool-> IO ()
testRandomGenList counter fCounter func expectedOut = if counter == fCounter then print (show fCounter ++ " tests passed")
                else do
                  (l1, l2) <- genTwoIntList
                  if  func l1 l2 == expectedOut then
                    do print ("pass on: " ++ show l1 ++ " and " ++ show l2)
                       testRandomGenList (counter+1) fCounter func expectedOut
                  else error ("failed test on: " ++ show l1 ++ " and " ++ show l2)

-- This function creates two random lists that are not the same
--      This should be improved by adding some filtering maybe
genTwoIntList :: IO ([Int], [Int])
genTwoIntList = do
  k <- getRandomInt 20
  n <- getRandomInt 10
  l1 <- getIntL k n
  k2 <- getRandomInt 20
  n2 <- getRandomInt 10
  l2 <- getIntL k n
  if l1 /= l2 then return (l1, l2) else genTwoIntList

-- =============== END OF CREATING AUTOMATED TESTING =================

-- ========== LECTURE BASED FUNCTIONS ============
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)

randomFlip :: Int -> IO Int
randomFlip x = do
   b <- getRandomInt 1
   if b==0 then return x else return (-x)


(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- The stronger function doesnt work for me yet.. forall isnt found and i cant find which package its imported from
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p
-- ========== END OF LECTURE USED FUNCTIONS ============

-- ============ Code from the previous Exercise ========= --
arrOfProps :: [([Char], Int -> Bool)]
arrOfProps = [("Prop Same Arrs", prop_baseCaseTrue),
              ("Prop Differnet Arrs", prop_baseCaseDifferent),
              ("Prop Valid Permutation", prop_validPerm),
              ("Prop Diff Lengths", prop_testDiffGeneratedLists)]

isEqual :: (Num a, Enum a) => (a -> Bool) -> (a -> Bool) -> Bool
isEqual leftProp rightProp = stronger [(-10)..10] leftProp rightProp && stronger [(-10)..10] rightProp leftProp

mapFunctions :: [([Char], Int)]
mapFunctions = reverse sorted where
    sorted = sortBy (compare `on` snd) rankedFunctions
    rankedFunctions = map getFunctionStrength arrOfProps

getFunctionStrength :: (a, Int -> Bool) -> (a, Int)
getFunctionStrength (functionName, leftProp) = (functionName, trueVals) where
    trueVals = length (filter (== True) arrOfBools) - 1
    arrOfBools =  map  (\(funcName, rightProp) -> stronger [(-10)..10] leftProp rightProp || isEqual leftProp rightProp) arrOfProps

showResults :: [Char]
showResults  = concatMap (\(left, right) -> left ++ " (" ++ show right ++ ") ,\n") mapFunctions
-- ============ End of Code from the previous Exercise ========= --

main :: IO ()
main = do 
      putStrLn "\n=== Testing 2 lists with the same value ===\n"
      quickCheck prop_baseCaseTrue
      putStrLn "\n=== Testing 2 lists with the different values ===\n"
      quickCheck prop_baseCaseDifferent
      putStrLn "\n=== Testing 2 lists with a valid permutaion ===\n"
      quickCheck prop_validPerm
      putStrLn "\n=== Testing 2 lists of different lengths ===\n"
      quickCheck prop_testDiffGeneratedLists
      putStrLn "\n=== Autogenerated Tests ===\n"
      prop_testRandomGeneratedPerms 100
      putStrLn "\n=== Showing the rankings of the props ===\n"
      putStrLn  showResults

-- Automated Testing
--  To automatically test the 'isPermutation' function we created a function which randomly 
--  generates two lists that are not the exact same. This is not a perfect implementaion as 
--  there is a chance that the two generated lists are a permuation of each other
--  to improve this function a filter mechanism would be needed

--  Additional automated testing:
--      We wanted to create a function that automatically generates a list and a permutation of
--      that list is created within the generator, this would test the function with many different 
--      permutations. However this did not work at the permutations function does not accept IO [Int]
--      types, or we lack the knowledge to implement it currently

-- Additional props to check
--      The sum of the lists could have been calcualted and compared, however this is not full proof
--      as you can have two seperate arrays of the same length and different content with the sum 
