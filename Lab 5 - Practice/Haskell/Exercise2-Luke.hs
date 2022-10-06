module Exercise2 where

import           Exercise1
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

countSurvivors 0 mutator properties fut = mutationResult
countSurvivors mutantNo mutator properties fut =
    results
        ++ mutationResult
        ++ countSurvivors (mutantNo - 1) mutator properties fut
  where
    results        = []
    mutationResult = evaluateMutations mutator properties fut

evaluateMutations mutator [] fut = do
    return (True)
evaluateMutations mutator (prop : properties) fut =
    mutatedValue && evaluateMutations mutator properties fut
  where
    mutatedValue = mutate mutator prop fut input
    input        = 10

-- countSurvivors 4000 [prop_tenElements, prop_firstElementIsInput], prop_sumIsTriangleNumberTimesInput
