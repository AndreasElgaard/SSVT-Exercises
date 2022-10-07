
module Exercise3 where
import           Data.List
import           MultiplicationTable
import           Mutation
import           Test.QuickCheck
import Exercise2
import Exercise1
import Data.Bool (bool)



-- Documentation of the approach:
--  Integer -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer])
    -- num mutants - list of properties - function under test (multiplication)

-- NO --- looping all the combinations of properties (permutations) and then, doing count_survivors?

-- MATRIX       Mutator 1   Mutator 2
-- Property 1   0           1
-- Property 2   1           0
-- Implementation

-- num mutants - list of mutators - list of properties - function under test (multiplication)
-- test = generate $ sequence $ replicate 20 (mutate removeElements prop_tenElements multiplicationTable 1)

-- is only one mutant now, not a list. but should be a list.

-- numMutants listMutants listProperties f
-- minimalSubsets:: Integer -> [[Integer] -> Gen [Integer]] -> [[Integer] -> Integer -> Bool]  -> (Integer -> [Integer]) -> [Integer]
-- minimalSubsets = propertyAxis

-- numMutants listMutators listproperties function
minimalSubsets:: Integer -> [[Integer] -> Gen [Integer]]  -> [(String, [Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen [(String, [Bool])]
minimalSubsets _ _ [] _ = return []
minimalSubsets numMutants listMutants (p:props) f = do
    mutants <- mutantsPropertyAxis numMutants listMutants p f
    propAxis <- minimalSubsets numMutants listMutants props f
    return(mutants : propAxis)


mutantsPropertyAxis:: Integer -> [[Integer] -> Gen [Integer]] -> (String, ([Integer] -> Integer -> Bool)) -> (Integer -> [Integer]) -> Gen (String, [Bool])
mutantsPropertyAxis _ [] (name, property) _ = return (name, [])
mutantsPropertyAxis numMutants (x:xs) (name, property) f = do
    survivorsCount <- countSurvivors numMutants x [property] f
    let boolCountCheck = survivorsCount > 0
    (name, tailListOfInt) <- mutantsPropertyAxis numMutants xs (name, property) f
    return (name, (boolCountCheck : tailListOfInt))

checker :: [(String, [Bool])] -> (String, Bool)
checker [] = ("", True)
checker ((nameX, x):(nameY, y):xs)
    | length x == 1 = (nameX, all (== False) x)
    | otherwise =  (setOfPropName, all (== False) processed && boolVal)
        where (_, boolVal) = checker ((setOfPropName,processed):xs)
              setOfPropName = nameX ++ ", " ++ nameY
              processed = map (\(left,right) -> left && right) zipped
              zipped = zip x y
checker ((nameX, x):y)
    | null y = ("", True)
    | otherwise =  (setOfPropName, all (== False) processed)
        where setOfPropName = nameX ++ ", " ++ nameY
              processed = map (\(left,right) -> left && right) zipped
              zipped = zip x headOfY
              (nameY, headOfY) = head y

zipResult= do
   let propsList = [("prop_firstElementIsInput", prop_firstElementIsInput) , ("prop_linear", prop_linear) , ("prop_moduloIsZero", prop_moduloIsZero) , ("prop_sumIsTriangleNumberTimesInput", prop_sumIsTriangleNumberTimesInput) , ("prop_tenElements", prop_tenElements) ]
   let mutatorList = [addElements, removeElements, multiplyByAListOfInts , multiplyElements , changeOrder , addForModulus , totallyRandom , changeRandomElement ]
   minList <- minimalSubsets 1 mutatorList propsList multiplicationTable
   let lengthP = length minList
   let subseqs = subsequences minList
   let ssss = filter (\(name,boolRes) -> boolRes && name /= "") (map checker subseqs)
   return ssss

--    return (length firstP, length secondP, length thirdP, length fourthP, length fifthP)
-- combinationCheck (x:xs) index = do

-- check (x:xs) counter =
--     !!

-- minimalSubsets:: Integer -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Integer
-- minimalSubsets numMutants props = goThroughAllPermutations 0 combinationOfProperties numMutants
-- where combinationOfProperties = permutations props

-- -- minimum amount of
-- goThroughAllPermutations:: Integer -> [([Integer] -> Integer -> Bool)] -> Integer (Integer -> [Integer]) -> [Integer] -> [Integer]
-- goThroughAllPermutations _ [] _ _ _ = result
-- goThroughAllPermutations min (x:xs) numMutants f
-- | min > survivors = goThroughAllPermutations survivors xs numMutants f x
-- |
-- | otherwise = goThroughAllPermutations min xs numMutants f
-- where survivors = count_survivors(numMutants x f)
