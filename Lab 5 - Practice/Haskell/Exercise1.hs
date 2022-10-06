module Exercise1 where
import           Data.List
import           MultiplicationTable
import           Mutation
import           Test.QuickCheck
import Data.Time.Format.ISO8601 (yearFormat)

test = generate $ sequence $ replicate 20 (mutate removeElements prop_tenElements multiplicationTable 1)

-- Time spend: x minutes --

-- Q: Write down which types of output are not yet covered by these mutators, and about their weakness/strength.
-- The currently implemented mutators alters the output of the function and have a low chance of testing
-- the function with a list of length 10, as expected from the properties.
-- We have listed some mutators which are believed to cover more test cases

-- 1. Reverse the list values.
-- This mutator uses the same length of the list (10) but alters the sequence of the content of the integer list
-- This mutator is relatively strong because it uses subset due to the fixed length and using the same content,
-- just in a differnet sequence.
changeOrder :: [Integer] -> Gen [Integer]
changeOrder xs = do
    return (reverse xs)

-- 2. Multiply values by a fixed random number.
-- Similiarly to the previous mutator this tests the same length of the output (10), in this case each element is multipled
-- by a random number. This test is weaker than the previous mutator but still relatively strong due to the fixed length of the list.
-- The rationale of this mutator involves changing the contents of the list to assure the tests are checking the output list baesd on
-- the input variable
multiplyElements :: [Integer] -> Gen [Integer]
multiplyElements xs = do
    num <- arbitrary :: Gen Integer
    return (map (* num) xs)

-- 3. Multiply list_1 with a list_2 of same length where list_1 index n is multiplied with list_2 index n.
-- This mutator generates a new list [1..10] and multiples each element with the output of the multiplcation table. For examples
-- Output of multiplicationTable 5  = [5,10,15,20,25,30,35,40,45,50]  multipled by [1,2,3,4,5,6,7,8,9,10] would output
-- [5,20,45,80,125,180,245,320,405,500], the first element of the list remains the same since it is multiplied by 1, but all other
-- elements are multiple by the respect number based on the index+1.
-- Similiarly to prevous mutators, this uses a fixed length of 10, but it is weaker since it caters for a larger subset.
multiplyByAListOfInts :: [Integer] -> Gen [Integer]
multiplyByAListOfInts xs = do
    let list = zip [1 .. 10] xs
    return (map (uncurry (*)) list)

-- 4. Add 1 to every element in the list.
-- This mutator aims to change each element in the output list by adding it by 1, this is done to change each number from an odd to an
-- even and viceversa. This is a very strong mutator as it is highly specific.
addForModulus :: [Integer] -> Gen [Integer]
addForModulus xs = do
    return (map (+ 1) xs)

-- 5. Randomly Generated Integer List
-- This is the weakest mutator as it generates a totally random list that may cater for a large set of outputs.
-- The rationale of this mutator relates to checking if tests can pass with an compeltely random set
-- NOTE: Since the 'prop_firstElementIsInput' property uses the head function, a complete random list cannot be generated
--  since head does not support blank lists (runtime error) such a finding reveals the property is not fully type safe beecause
--  a blank list could not be handled by such a property
totallyRandom :: [Integer] -> Gen [Integer]
totallyRandom xs = do
    (arbitrary :: Gen [Integer]) `suchThat` (not . null)

-- Extra mutator
-- Replaces one element random element in the list with a random nunmber
changeRandomElement :: [Integer] -> Gen [Integer]
changeRandomElement xs = do
    numLength <- choose (1, length xs)
    num       <- arbitrary :: Gen Integer
    let (first, second) = splitAt numLength xs
    let tailOf          = customTail second
    return (first ++ [num] ++ tailOf)

-- Helper function to handle tailing of empty lists
customTail :: [a] -> [a]
customTail []       = []
customTail (x : xs) = xs


-- Helper funcition to get tail
checkMonad :: Maybe Bool -> Maybe Bool -> Bool
checkMonad (Just True) (Just True) = True
checkMonad x y = False
