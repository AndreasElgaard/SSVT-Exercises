module Exercise5 where
import           Data.List
import           MultiplicationTable
import           Mutation
import           Test.QuickCheck
import Exercise2
import Exercise1

conjectures:: Integer -> [[Integer] -> Gen [Integer]]  -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> Gen [[Bool]]
conjectures _ _ [] _ = return []
conjectures numMutants listMutants (p:props) f = do
    mutants <- mutantsPropertyAxis numMutants listMutants p f
    propAxis <- conjectures numMutants listMutants props f
    return(mutants : propAxis)


mutantsPropertyAxis:: Integer -> [[Integer] -> Gen [Integer]] -> ([Integer] -> Integer -> Bool) -> (Integer -> [Integer]) -> Gen [Bool]
mutantsPropertyAxis _ [] _ _ = return []

mutantsPropertyAxis numMutants (x:xs) property f = do
    survivorsCount <- countSurvivors numMutants x [property] f
    let boolCountCheck = (if survivorsCount > 0 then True else False)
    tailListOfInt <- mutantsPropertyAxis numMutants xs property f
    return (boolCountCheck : tailListOfInt)

checkConj [] = []
checkConj (x:xs) = do
    tailing <- checkConj xs
    nonMonadic <- x
    return (all (== True) nonMonadic : tailing)
