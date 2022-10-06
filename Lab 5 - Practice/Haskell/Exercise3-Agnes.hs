
module Exercise3 where
import           Data.List
import           MultiplicationTable
import           Mutation
import           Test.QuickCheck
import Exercise2
import Exercise1



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
minimalSubsets:: Integer -> [[Integer] -> Gen [Integer]]  -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> Gen [[Bool]]
minimalSubsets _ _ [] _ = return []
minimalSubsets numMutants listMutants (p:props) f = do
    mutants <- mutantsPropertyAxis numMutants listMutants p f
    propAxis <- minimalSubsets numMutants listMutants props f
    return(mutants : propAxis)


mutantsPropertyAxis:: Integer -> [[Integer] -> Gen [Integer]] -> ([Integer] -> Integer -> Bool) -> (Integer -> [Integer]) -> Gen [Bool]
mutantsPropertyAxis _ [] _ _ = return []

mutantsPropertyAxis numMutants (x:xs) property f = do
    survivorsCount <- countSurvivors numMutants x [property] f
    let boolCountCheck = (if survivorsCount > 0 then True else False)
    tailListOfInt <- mutantsPropertyAxis numMutants xs property f
    return (boolCountCheck : tailListOfInt)

zipResult= do
   minList <- minimalSubsets 1 [addElements, removeElements, multiplyByAListOfInts , multiplyElements , changeOrder , addForModulus , totallyRandom , changeRandomElement ] [prop_firstElementIsInput , prop_linear , prop_moduloIsZero , prop_sumIsTriangleNumberTimesInput , prop_tenElements ] multiplicationTable
   let lengthP = length minList
   let (firstP:secondP:thirdP:fourthP:fifthP:[]) =  minList
--    let x =
--    let zipped = map (\n -> all (== True) n) (subsequences minList)
   let subseqs = subsequences minList
   let ssss = map () subseqs
   return ssss

checker1 :: [[Bool]] -> Bool
checker1 [] = False
checker1 (x:y:xs)
    | length x == 1 = all (== False) x
    | otherwise =  all (== False) processed && (checker1 processed:xs) ---- Check here for fuck up
        where processed = (map (\(left,right) -> left || right) zipped)
              zipped = zip x y
checker1 (x:y) = all (== False) processed
        where processed = map (\(left,right) -> left || right) zipped
              zipped = zip x y
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
