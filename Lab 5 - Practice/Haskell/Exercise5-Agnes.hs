module Exercise5 where
import           Data.List
import           MultiplicationTable
import           Mutation
import           Test.QuickCheck
import Exercise2
import Exercise1

-- numMutants listMutators properties function
conjectures:: Integer -> [[Integer] -> Gen [Integer]]  -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> Gen [[Bool]]
conjectures _ _ [] _ = return []
conjectures numMutants listMutants (p:props) f = do
    mutants <- mutantsPropertyAxis numMutants listMutants p f
    propAxis <- conjectures numMutants listMutants props f
    return(mutants : propAxis)


porcentageOfKills:: Integer -> Gen [[Integer]] -> Gen [Integer]
porcentageOfKills numMutants survivors = do 
     listOfSurvivors <- survivors
     let implication = implication listOfSurvivors
     return finalNumber



-- right now it-s just comparing if the lists are the same, but i should do two recursions.
implication::[[Integer]] -> [[Integer]]
implication  _ _ [] = []
implication (s:s2:survivors) 
    | zip s s2 = [s1:s2]: implication (s2:survivors)
    | otherwise implication (s2:survivors)

checkConj [] = []
checkConj (x:xs) = do
    tailing <- checkConj xs
    nonMonadic <- x
    return (all (== True) nonMonadic : tailing)
