module Exercise4 where
import Data.List
import Data.Char
-- import System.Random
import Test.QuickCheck
import System.Random ( getStdRandom, Random(randomR) )
import qualified Control.Monad.IO.Class
import Data.Function ( on )


isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation  = isPermSort

-- Assuming that the list can have duplicates, this would probably work best
isPermSort :: Ord a => [a] -> [a] -> Bool
isPermSort [] [] = True
isPermSort xs xsPerm = length xs == length xsPerm && sort xs == sort xsPerm

-- If the list does not have duplicates: (This fails with duplicates)
isPermElems :: Eq a => [a] -> [a] -> Bool
isPermElems [] [] = False
isPermElems x [] = True
isPermElems xs (x: xsPerm)
    | x `elem` xs = isPermElems xs xsPerm
    | otherwise = False

-- =============== PROPS ============
-- Probably the best prop would be to comapes [1..] to [1..] but thats impossible lols
-- Testing the same list input as a base case

prop_baseCaseTrue :: Int -> Bool
prop_baseCaseTrue start  =  isPermutation x x where
    x = [start..end]
    end = start+10

-- Testing differnet lists 
prop_baseCaseDifferent :: Int -> Bool
prop_baseCaseDifferent start =  not (isPermutation l1 l2) where
    l1 = [start..end]
    l2 = [(start+10)..(end+10)]
    end = start+10

prop_validPerm :: Int -> Bool
prop_validPerm start = isPermutation [start..end] headOfPerms where
    (headOfPerms:_) = permutations [start..end]
    end = start+10

-- Generate 2 lists with different lengths and make sure they are not equal
prop_testDiffGeneratedLists :: Int -> Bool
prop_testDiffGeneratedLists start = not (isPermutation l1 l2) where
    l1 = [start..end]
    l2 = [start..end-1]
    end = start+10

-- Generates n tests that randomly generates 2 lists of ints to test isPermutation
prop_testRandomGeneratedPerms :: Int -> IO ()
prop_testRandomGeneratedPerms n = testRandomGenList 1 n isPermutation False
-- =============== END OF PROPS  ================

-- =============== CREATING  AUTOMATED TESTING =================
-- BIG KAVIAT WITH THIS FUNCTION
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



arrOfProps :: [([Char], Int -> Bool)]
arrOfProps = [("Prop Same Arrs", prop_baseCaseTrue),
              ("Prop Differnet Arrs", prop_baseCaseDifferent),
              ("Prop Valid Permutation", prop_validPerm),
              ("Prop  Diff Lengths", prop_testDiffGeneratedLists)]

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

-- ========== TESTING STRENGTH OF PROPS ============

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
    --   putStrLn "\n=== Autogenerated Tests===\n"
    --   testRandomGenList
      putStrLn "\n=== Showing the rankings of the props ===\n"
      putStrLn  showResults

