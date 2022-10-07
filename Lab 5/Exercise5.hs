module Exercise5 where
import           Data.List
import           MultiplicationTable
import           Mutation
import           Test.QuickCheck
import Exercise2
import Exercise1

conjectures:: Integer -> [[Integer] -> Gen [Integer]]  -> [(String, [Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen [(String, [Bool])]
conjectures _ _ [] _ = return []
conjectures numMutants listMutants (p:props) f = do
    mutants <- mutantsPropertyAxis numMutants listMutants p f
    propAxis <- conjectures numMutants listMutants props f
    return(mutants : propAxis)

mutantsPropertyAxis:: Integer -> [[Integer] -> Gen [Integer]] -> (String, ([Integer] -> Integer -> Bool)) -> (Integer -> [Integer]) -> Gen (String, [Bool])
mutantsPropertyAxis _ [] (name, property) _ = return (name, [])
mutantsPropertyAxis numMutants (x:xs) (name, property) f = do
    survivorsCount <- countSurvivors numMutants x [property] f
    let boolCountCheck = survivorsCount > 0
    (name, tailListOfInt) <- mutantsPropertyAxis numMutants xs (name, property) f
    return (name, (boolCountCheck : tailListOfInt))

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


resultOfEquivalence :: Gen [([Char], Bool)]
resultOfEquivalence= do
   let propsList = [("prop_firstElementIsInput", prop_firstElementIsInput) , ("prop_linear", prop_linear) , ("prop_moduloIsZero", prop_moduloIsZero) , ("prop_sumIsTriangleNumberTimesInput", prop_sumIsTriangleNumberTimesInput) , ("prop_tenElements", prop_tenElements) ]
   let mutatorList = [addElements, removeElements, multiplyByAListOfInts , multiplyElements , changeOrder , addForModulus , totallyRandom , changeRandomElement ]
   matrix <- conjectures 1 mutatorList propsList multiplicationTable
   let subseqs = subsequences matrix
   let equivalents = filter (\(name,boolRes) -> boolRes && name /= "") (map equivalenceCheck subseqs)
   return equivalents

resultOfImplication :: Gen [([Char], Bool)]
resultOfImplication= do
   let propsList = [("prop_firstElementIsInput", prop_firstElementIsInput) , ("prop_linear", prop_linear) , ("prop_moduloIsZero", prop_moduloIsZero) , ("prop_sumIsTriangleNumberTimesInput", prop_sumIsTriangleNumberTimesInput) , ("prop_tenElements", prop_tenElements) ]
   let mutatorList = [addElements, removeElements, multiplyByAListOfInts , multiplyElements , changeOrder , addForModulus , totallyRandom , changeRandomElement ]
   matrix <- conjectures 1 mutatorList propsList multiplicationTable
   let subseqs = subsequences matrix
   let implications = filter (\(name,boolRes) ->  boolRes && name /= "") (map implicationCheck subseqs)
   return implications

testingEquivalence :: Gen [([Char], Bool)]
testingEquivalence= do
   let propsList = [("prop_firstElementIsInput", prop_firstElementIsInput) , ("prop_firstElementIsInput", prop_firstElementIsInput) ]
   let mutatorList = [addElements, removeElements, multiplyByAListOfInts , multiplyElements , changeOrder , addForModulus , totallyRandom , changeRandomElement ]
   matrix <- conjectures 1 mutatorList propsList multiplicationTable
   let subseqs = subsequences matrix
   let equivalents = filter (\(name,boolRes) -> boolRes && name /= "") (map equivalenceCheck subseqs)
   return equivalents

