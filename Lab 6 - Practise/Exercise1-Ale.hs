module Exercise1 where
import System.Random ( randomRIO )
import SetOrd ( Set(..) )
import Test.QuickCheck ( Arbitrary(arbitrary), Gen )
import Data.List(nub)

-- Time Spents: 120 mins.... longer then I would like to admit lol
-- ================== Generator From Scratch ================= --
-- Function process
-- 1. This generator uses randomRIO to generate the length of the list, the maximum lenght of the list is 20 in this case
-- 2. A random number is generated in each iteration, if the number already exists in the list a new number is generated.
-- 3. The number is added to the output list
-- To test the generator call 'generateRandomList'
-- Note: The random numbers are generated from the range of -200 to 200, this range is arbitrary and can be changed as needed
generateRandomList :: IO (Set Int)
generateRandomList = do
    length <- randomRIO (0,20) -- Length of the output array
    newSet <- generateWCounter length []
    return (Set newSet)

-- Generates a unique number and appends it to the list, pattern matched for all different possiblities of the counter and the output list
generateWCounter :: Int -> [Int] ->  IO [Int]
generateWCounter 0 outList = do return []
generateWCounter counter [] = do
    randNo <- randomRIO (-200,200)
    newNum <- uniqueNum randNo []
    list <- generateWCounter(counter-1) []
    return (newNum : list)
generateWCounter counter outList = do
    randNo <- randomRIO (-200,200)
    newNum <- uniqueNum randNo outList
    list <- generateWCounter (counter-1) (tail outList)
    return (newNum : list)

-- Checks if the given number is in the list, this function is made to improve code readability
uniqueNum :: Int -> [Int] -> IO Int
uniqueNum inputNumber list = do
    isInElem (inputNumber `elem` list) inputNumber list

-- Checks if the element is in the list, if True a new random number is generated
isInElem :: Bool -> Int -> [Int] -> IO Int
isInElem True inNum outList = do
    rand <- randomRIO (-200,200)
    uniqueNum rand outList
isInElem False inNum outList = do return inNum

-- ================ QUICKCHECK Generator ==================--
-- Generates a list of number using QuickCheck generators and applies it to the Set datatype, using nub to only accept unique numbers
generateSets :: Gen (Set Int)
generateSets = (arbitrary :: Gen [Int]) >>= \x -> return $ Set (nub x)



