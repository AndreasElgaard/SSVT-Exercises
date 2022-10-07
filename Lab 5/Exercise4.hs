module Exercise4 where
import Data.List
import MultiplicationTable
import Mutation
import Test.QuickCheck
import Exercise2
import Exercise1

-- Time spend: 90 minutes --

-- Implement a function that calculates the strength of a given set of properties, 
-- which is the percentage of mutants they kill.

-- =================================== DOCUMENTATION OF APPROACH ===================================
-- In the previous exercise 3, we had created a function that created a very useful matrix. This
-- matrix gave us the number of survivors a property had for every single mutator. This matrix
-- resulted in:
--  Matrix  Mutator 1   Mutator 2 ....
--  Prop 1  0 survivors 10 survivors
--  Prop 2  5 survivors 0 survivors
-- For this exercise, we want the procentage of these results in the matrix.
-- We will input the total number of mutants and the result of this matrix.
-- Since the matrix is in a monad, we will need to be aware that by using te do
-- structure. The steps would be the following:
--      1. Unwrap the monadic matrix (list of lists) with the survivors counts per property and mutator.
--      2. For all the lists within the matrix, we will sum its values. E.g: [[0,2,10],[1,2,3]] = [12,6]
--      3. We need to obtain how many mutators have been used, this can be done just by obtaining the length
--         of one list within the matrix. E.g: [[0,2,10],[1,2,3]] - the number of mutators is 3.
--      4. We, then, multiply the number of mutators with the number of mutants we used when creating the matrix.
--         This total number of mutants is one of the inputs of our function. 
--         E.g: numMutants = 10, matrix = [[0,2,10],[1,2,3]] --> multipliedNumber = 3*10 =30
--      5. We multiply the list of summed survivors per property by 100 first, to avoid big approximations 
--         since we are using integers. E.g: [12,6] * 100 = [1200,600]
--      6. Then, we divide this list by the the multiplied number of number of mutators and mutants.
--         E.g: [1200,600] `div` 30 --> [40,20]
--      7. This value is the porcenatge of survivors, since we want the procentage of kills, we
--         substract this value to 100. E.g: [60,80]
--      7. We wrap this final array into a Monad and this is our output.

--  This is the end of the function and now we have the percentage of the mutants each property kills
--  this defines the strength of the property. The more mutants a property kills, the stronger it is.

-- =================================== IMPLEMENTATION ===================================

-- It returns a list of the porcentages per property [%prop1,%prop2,...]. The porcentage is
-- the amount of mutants they kill. 
porcentageOfKills:: Integer -> Gen [[Integer]] -> Gen [Integer]
porcentageOfKills numMutants survivors = do 
     listOfSurvivors <- survivors
     let sumOfSurvivors = map sum listOfSurvivors 
     let lengthMutators = toInteger (length (head listOfSurvivors))
     let total = lengthMutators * numMutants
     let multiply = map (*100) sumOfSurvivors
     let porcentageSurvivors =  map (`div` total) multiply
     let finalNumber = map (`subtract`100) porcentageSurvivors
     return finalNumber


-- If you want to exectue this:

-- exercise3function = minimalSubsets 10 [addElements, removeElements, multiplyByAListOfInts , multiplyElements , changeOrder , addForModulus , totallyRandom , changeRandomElement ] [prop_firstElementIsInput , prop_linear , prop_moduloIsZero , prop_sumIsTriangleNumberTimesInput , prop_tenElements ] multiplicationTable
-- generate $ porcentageOfKills 10 exercise3function




-- In the previous exercise, we created an extra step to make things easier to understand
-- and that was that if the survivors counts was greater than 0, we would just keep 1, instead of 5,14 
-- and numbers like such. Since this unables us to reuse this function, we just copied the same
-- code from Exercise 3 below and deleteed this extra line of code that reworked the numbers. 

-- =================================== PREVIOUS CODE ===================================


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