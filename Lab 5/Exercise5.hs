module Exercise5 where
import           Data.List
import           MultiplicationTable
import           Mutation
import           Test.QuickCheck
import Exercise2
import Exercise1

-- Time spend: 40 minutes --

-- Implement function(s) that calculate the conjectures: properties that are equivent,
-- whose cases are subsets of other properties, etc.

-- =================================== DOCUMENTATION OF APPROACH ===================================

-- To find conjectures, we will use a similiar procedure as exercise 3. 
--        1. First, we will create a matrix that for each property shows us all the mutators
--           it killed. This is the matrix we created in exercise 3, and a similar version to
--           the one we use in Exercise 4.
--           -- E.g: A possible result for 2 properties 3 mutators
--               would be [[True,True,False],[True,False,True]]
--        2. Then, we create all the subsequences (all combinations) of this matrix
--        3. Afterwars, we will map all these values to two different check functions 
--           (one for equivalence and the other for implication)
--        4. These will return a list of tuples of the name of the property and its value (True or False)
--           for all the properties that ended up being True after the check.

-- =================================== IMPLEMENTATION ===================================

-- Both these functions works the same as in Exercise 3. It creates a matrix, where for each property, 
-- all mutators are tested. The intersection between property and mutator is False if 
-- for all mutants generated with said mutator, property sucessfully kills mutants,
-- otherwise the intersection's value is True.


-- This is where we grab all the inputs and use the function mutantsPropertyAxis for
-- each property recursively.
conjectures:: Integer -> [[Integer] -> Gen [Integer]]  -> [(String, [Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen [(String, [Bool])]
conjectures _ _ [] _ = return []
conjectures numMutants listMutants (p:props) f = do
    mutants <- mutantsPropertyAxis numMutants listMutants p f
    propAxis <- conjectures numMutants listMutants props f
    return(mutants : propAxis)

-- This recursive function goes through all mutators and uses the function "countSurvivors",
-- which as we explained in Ex2 returns the number of mutants tests that a property survives. 
mutantsPropertyAxis:: Integer -> [[Integer] -> Gen [Integer]] -> (String, ([Integer] -> Integer -> Bool)) -> (Integer -> [Integer]) -> Gen (String, [Bool])
mutantsPropertyAxis _ [] (name, property) _ = return (name, [])
mutantsPropertyAxis numMutants (x:xs) (name, property) f = do
    survivorsCount <- countSurvivors numMutants x [property] f
    let boolCountCheck = survivorsCount > 0
    (name, tailListOfInt) <- mutantsPropertyAxis numMutants xs (name, property) f
    return (name, (boolCountCheck : tailListOfInt))


-- =================================== CONJECTURES ===================================
-- Both conjecture implementations and exercise 3 checker function are identical but 
-- have a difference in the way we map the zipped lists. When I say lists I mean 
-- whichever combination of the matrix we are going through at the time. 

-- Conjecture 1: Equivalence.
-- Checks recursively, for all subsequences of property sets, if two properties kill
-- the same mutants for mutator list specified.

equivalenceCheck :: [(String, [Bool])] -> (String, Bool)
equivalenceCheck [] = ("", True)
equivalenceCheck ((nameX, x):(nameY, y):xs)
    | length x == 1 = (nameX, all (== True) x)
    | otherwise =  (setOfPropName, all (== True) processed && boolVal)
        where (_, boolVal) = equivalenceCheck ((setOfPropName,processed):xs)
              setOfPropName = nameX ++ ", " ++ nameY
              processed = map (\(left,right) -> left == right) zipped
              zipped = zip x y
equivalenceCheck ((nameX, x):y)
    | null y = ("", True)
    | otherwise =  (setOfPropName, all (== True) processed)
        where setOfPropName = nameX ++ ", " ++ nameY
              processed = map (\(left,right) -> left == right) zipped
              zipped = zip x headOfY
              (nameY, headOfY) = head y

-- Conjecture 2: Implication.
-- Checks recursively, for all subsequences of property sets, if a property kills
-- all the mutants that another property kills.

implicationCheck :: [(String, [Bool])] -> (String, Bool)
implicationCheck [] = ("", True)
implicationCheck ((nameX, x):(nameY, y):xs)
    | length x == 1 = (nameX, all (== True) x)
    | otherwise =  (setOfPropName, all (== True) processed && boolVal)
        where (_, boolVal) = implicationCheck ((setOfPropName,processed):xs)
              setOfPropName = nameY ++ ", " ++ nameX
              processed = map (\(left,right) -> if(not right) then left == right else True) zipped
              zipped = zip x y
implicationCheck ((nameX, x):y)
    | null y = ("", True)
    | otherwise =  (setOfPropName, all (== True) processed)
        where setOfPropName = nameY ++ ", " ++ nameX
              processed = map (\(left,right) -> if(not right) then left == right else True) zipped
              zipped = zip x headOfY
              (nameY, headOfY) = head y

-- =================================== Results ===================================
-- The results are explained in the documentation of the approach. 
-- Basically, a variable is created to get all the props, another for all the mutators.
-- We create the matrix and its subsequences.
-- And for all subsequences, we map them using  a conjecture function

-- Here we check for Equivalence:
resultOfEquivalence :: Gen [([Char], Bool)]
resultOfEquivalence= do
   let propsList = [("prop_firstElementIsInput", prop_firstElementIsInput) , ("prop_linear", prop_linear) , ("prop_moduloIsZero", prop_moduloIsZero) , ("prop_sumIsTriangleNumberTimesInput", prop_sumIsTriangleNumberTimesInput) , ("prop_tenElements", prop_tenElements) ]
   let mutatorList = [addElements, removeElements, multiplyByAListOfInts , multiplyElements , changeOrder , addForModulus , totallyRandom , changeRandomElement ]
   matrix <- conjectures 1 mutatorList propsList multiplicationTable
   let subseqs = subsequences matrix
   let equivalents = filter (\(name,boolRes) -> boolRes && name /= "") (map equivalenceCheck subseqs)
   return equivalents

-- Here for Implication: 
resultOfImplication :: Gen [([Char], Bool)]
resultOfImplication= do
   let propsList = [("prop_firstElementIsInput", prop_firstElementIsInput) , ("prop_linear", prop_linear) , ("prop_moduloIsZero", prop_moduloIsZero) , ("prop_sumIsTriangleNumberTimesInput", prop_sumIsTriangleNumberTimesInput) , ("prop_tenElements", prop_tenElements) ]
   let mutatorList = [addElements, removeElements, multiplyByAListOfInts , multiplyElements , changeOrder , addForModulus , totallyRandom , changeRandomElement ]
   matrix <- conjectures 1 mutatorList propsList multiplicationTable
   let subseqs = subsequences matrix
   let implications = filter (\(name,boolRes) ->  boolRes && name /= "") (map implicationCheck subseqs)
   return implications

-- Here we write a case where we force an equivalence, to facilitate if the implementation of equivalence
-- gives the proper results. We do this by using the comparing the same property. 

testingEquivalence :: Gen [([Char], Bool)]
testingEquivalence= do
   let propsList = [("prop_firstElementIsInput", prop_firstElementIsInput) , ("prop_firstElementIsInput", prop_firstElementIsInput) ]
   let mutatorList = [addElements, removeElements, multiplyByAListOfInts , multiplyElements , changeOrder , addForModulus , totallyRandom , changeRandomElement ]
   matrix <- conjectures 1 mutatorList propsList multiplicationTable
   let subseqs = subsequences matrix
   let equivalents = filter (\(name,boolRes) -> boolRes && name /= "") (map equivalenceCheck subseqs)
   return equivalents

