module Exercice3 where
import           Data.List
import           MultiplicationTable
import           Mutation
import           Test.QuickCheck



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
minimalSubsets:: Integer -> [[Integer] -> Gen [Integer]] -> [[Integer] -> Integer -> Bool]  -> (Integer -> [Integer]) -> [Integer]
minimalSubsets = propertyAxis 

-- numMutants listMutators listproperties function
propertyAxis:: Integer -> [[Integer] -> Gen [Integer]]  -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [Integer]
propertyAxis _ _ [] _ = []
propertyAxis numMutants listMutants (p:props) f = mutants ++ propertyAxis numMutants listMutants props f
    where mutants = mutantsAxis numMutants listMutants p f


mutantsAxis:: Integer -> [[Integer] -> Gen [Integer]] -> ([Integer] -> Integer -> Bool) -> (Integer -> [Integer]) -> [Integer]
mutantsAxis _ [] _ _ = []
mutantsAxis numMutants (x:xs) property f = survivorsCount ++ mutantsAxis numMutants xs property f
    where survivorsCount = fakeCountSurviors

fakeCountSurviors:: [Integer]
fakeCountSurviors = [0]
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
    