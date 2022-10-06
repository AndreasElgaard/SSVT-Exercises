module Exercise4 where
import Data.List
import MultiplicationTable
import Mutation
import Test.QuickCheck
import Exercise2
import Exercise1

-- Time spend: x minutes --

-- =================================== DOCUMENTATION OF APPROACH ===================================
-- 

-- =================================== IMPLEMENTATION ===================================
mutatorsTemp = [addElements, removeElements, multiplyByAListOfInts , multiplyElements , changeOrder , addForModulus , totallyRandom , changeRandomElement ]
props = [prop_firstElementIsInput ]
temp = generate $ minimalSubsets 10000 mutatorsTemp props multiplicationTable


porcentageOfKills:: Integer -> Gen [[Integer]] -> Gen [Integer]
porcentageOfKills numMutants survivors = do 
     listOfSurvivors <- survivors
     let sumOfSurvivors = map sum listOfSurvivors 
     let finalNumber =  map (*100) sumOfSurvivors
     return (map (div numMutants) finalNumber)



-- =================================== HELPERS ===================================
  -- numMutants listMutators listproperties function
minimalSubsets:: Integer -> [[Integer] -> Gen [Integer]] -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> Gen [[Integer]]
minimalSubsets _ _ [] _ = return []
minimalSubsets numMutants listMutants (p:props) f = do
    mutants <- mutantsPropertyAxis numMutants listMutants p f
    propAxis <- minimalSubsets numMutants listMutants props f
    return(mutants : propAxis)


mutantsPropertyAxis:: Integer -> [[Integer] -> Gen [Integer]] -> ([Integer] -> Integer -> Bool) -> (Integer -> [Integer]) -> Gen [Integer]
mutantsPropertyAxis _ [] _ _ = return []
mutantsPropertyAxis numMutants (x:xs) property f = do
    survivorsCount <- countSurvivors numMutants x [property] f
    tailListOfInt <- mutantsPropertyAxis numMutants xs property f
    return (survivorsCount : tailListOfInt)