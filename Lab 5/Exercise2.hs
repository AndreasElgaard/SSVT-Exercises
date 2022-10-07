module Exercise2 where

import           Exercise1
import           GhcPlugins
import           MultiplicationTable
import           Mutation
import           Test.QuickCheck

-- Time spend: 180 minutes --

-- =================================== DOCUMENTATION OF APPROACH ===================================
-- Write a function that counts the number of survivors and document the effect of which
-- mutations are used and which properties are used on the number of survivors.

-- =================================== IMPLEMENTATION ===================================
-- Function that counts the survivors in a function under test
-- The first argument is the number of mutants (4000 in the FitSpec example).
-- The second argument is the list of properties.
-- The third argument is the function under test (the multiplication table function in this case).
-- The output is the number of surviving mutants (0 in the FitSpec example).

-- Method will filter the list of mutants which survive (i.e. true values) under the list of
-- properties passed, and return total no. of mutants surviving.
countSurvivors
    :: Integer
    -> ([Integer] -> Gen [Integer])
    -> [[Integer] -> Integer -> Bool]
    -> (Integer -> [Integer])
    -> Gen Integer
countSurvivors mutantNo mutator properties fut = do
    xs <- sequence $ createMutatedSurvivorsList mutantNo mutator properties fut
    let filtered = filter (== True) xs
    return (toInteger (length filtered))

-- Recursively chechking for mutants surviving up until all mutants (according to mutant no specified)
-- have been exhausted.
createMutatedSurvivorsList
    :: Integer
    -> ([Integer] -> Gen [Integer])
    -> [[Integer] -> Integer -> Bool]
    -> (Integer -> [Integer])
    -> [Gen Bool]
createMutatedSurvivorsList 0        mutator properties fut = []
createMutatedSurvivorsList mutantNo mutator properties fut = do
    evaluateMutations mutator properties fut inputNumber
        : createMutatedSurvivorsList (mutantNo - 1) mutator properties fut
    where inputNumber = 20 -- This can be amended to whatever random value
                           -- one wants to try generate mult table (fut) for

-- Returns all true surviving mutations
evaluateMutations
    :: Eq a
    => (a -> Gen a)
    -> [a -> Integer -> Bool]
    -> (Integer -> a)
    -> Integer
    -> Gen Bool
evaluateMutations mutator props fut inputNumber = do
    mutatedValue <- mutate' mutator props fut inputNumber
    return (all (== True) mutatedValue)

-- This was used prior to revision of mutate function, to allow for deterministic comparison
-- i.e. constant mutators to compare properties

-- evaluateMutations
--     :: Eq a
--     => (a -> Gen a)
--     -> [a -> Integer -> Bool]
--     -> (Integer -> a)
--     -> Integer
--     -> Gen (Maybe Bool)
-- evaluateMutations mutator [] fut inputNumber = do
--     return (Just True)
-- evaluateMutations mutator (prop : properties) fut inputNumber = do
--     mutatedValue           <- mutate' mutator prop fut inputNumber
--     evaluationMutationsRec <- evaluateMutations mutator
--                                                 properties
--                                                 fut
--                                                 inputNumber
--     return (checkMonadComp mutatedValue evaluationMutationsRec)

-- -- Helper function to compare Monadic bools - no longer used
-- checkMonadComp :: Maybe Bool -> Maybe Bool -> Maybe Bool
-- checkMonadComp (Just True) (Just True) = Just True
-- checkMonadComp x           y           = Just False

-- Function that returns a list of properties
properties :: [[Integer] -> Integer -> Bool]
properties =
    [ prop_firstElementIsInput
    , prop_tenElements
    , prop_sumIsTriangleNumberTimesInput
    , prop_linear
    , prop_moduloIsZero
    ]

-- =================================== TEST ===================================
-- Counting survivors in addElements mutator
countSurviviorsInAddElements =
    generate $ countSurvivors 4000 addElements properties multiplicationTable

-- Counting survivors in removeElements mutator
countSurviviorsInRemoveElements =
    generate $ countSurvivors 4000 removeElements properties multiplicationTable

-- Counting survivors in changeOrder mutator
countSurviviorsInChangeOrder =
    generate $ countSurvivors 4000 changeOrder properties multiplicationTable

-- Counting survivors in multiplyElements mutator
countSurviviorsInMultiplyElements = generate
    $ countSurvivors 4000 multiplyElements properties multiplicationTable

-- Counting survivors in multiplyByAListOfInts mutator
countSurviviorsInMultiplyByAListOfInts = generate $ countSurvivors
    4000
    multiplyByAListOfInts
    properties
    multiplicationTable

-- Counting survivors in addForModulus mutator
countSurviviorsInAddForModulus =
    generate $ countSurvivors 4000 addForModulus properties multiplicationTable

-- Counting survivors in changeRandomElement mutator
countSurviviorsInChangeRandomElement = generate
    $ countSurvivors 4000 changeRandomElement properties multiplicationTable
