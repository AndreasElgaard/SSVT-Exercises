module Exercise2 where

import           Exercise1
import           GhcPlugins                     ( trueDataCon )
import           MultiplicationTable
import           Mutation
import           Test.QuickCheck

-- countSurvivors :: Integer -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Integer

-- Where the first argument is the number of mutants (4000 in the FitSpec example).
-- And the second argument is the list of properties.
-- And the third argument is the function under test (the multiplication table function in this case).
-- And the output is the number of surviving mutants (0 in the FitSpec example).

-- Document the effect of which mutations are used and which properties are used on the number of survivors.
-- Hint: Consider the relation between properties and survivors.

-- Another hint: The above-mentioned function definition is not final. Feel free to modify it, for example by adding the mutations that should be used.

-- Deliverables: implementation, documentation of approach, effect of using different mutators/properties, indication of time spent.#

-- showCount = map ()

createMutatedSurvivorsList
    :: Integer
    -> ([Integer] -> Gen [Integer])
    -> [[Integer] -> Integer -> Bool]
    -> (Integer -> [Integer])
    -> [Gen (Maybe Bool)]
createMutatedSurvivorsList 0 mutator properties fut = [mutationResult]
  where
    mutationResult = evaluateMutations mutator properties fut inputNumber
    inputNumber    = 20

createMutatedSurvivorsList mutantNo mutator properties fut = do
    evaluateMutations mutator properties fut inputNumber
        : createMutatedSurvivorsList (mutantNo - 1) mutator properties fut
    where inputNumber = 20

countSurvivors
    :: Integer
    -> ([Integer] -> Gen [Integer])
    -> [[Integer] -> Integer -> Bool]
    -> (Integer -> [Integer])
    -> Gen Int
countSurvivors mutantNo mutator properties fut = do
    xs <- sequence $ createMutatedSurvivorsList mutantNo mutator properties fut
    let filtered = filter (\x -> x == Just True) xs
    return (length filtered)

evaluateMutations
    :: Eq a
    => (a -> Gen a)
    -> [a -> Integer -> Bool]
    -> (Integer -> a)
    -> Integer
    -> Gen (Maybe Bool)
evaluateMutations mutator [] fut inputNumber = do
    return (Just True)
evaluateMutations mutator (prop : properties) fut inputNumber = do
    mutatedValue           <- mutate mutator prop fut inputNumber
    evaluationMutationsRec <- evaluateMutations mutator
                                                properties
                                                fut
                                                inputNumber
    return (checkMonadComp mutatedValue evaluationMutationsRec)



checkMonadComp :: Maybe Bool -> Maybe Bool -> Maybe Bool
checkMonadComp (Just True) (Just True) = Just True
checkMonadComp x           y           = Just False
-- countSurvivors 4000 [prop_tenElements, prop_firstElementIsInput], prop_sumIsTriangleNumberTimesInput

