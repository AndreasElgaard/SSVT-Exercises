module Exercise1 where

import           Data.Char
import           Data.List
import           Lecture3
import           Test.QuickCheck

-- =================================== Implementation ===================================
-- Function that checks if a formula is a contradiction.
-- A formula is a contradiction if is not satisfied for any values
-- (i.e. False for every True, False combination possible).
contradiction :: Form -> Bool
contradiction = not . satisfiable

-- Function that checks if a formula is a tautology.
-- A formula is a tautology if all values are satisfied.
-- (i.e. True for every True, False combination possible, ex. P || Not (P) )).
tautology :: Form -> Bool
tautology f = all (`evl` f) (allVals f)

-- Function that checks if a formula f1 entails formula f2 if and only if
-- every True, False combination evaluation as TRUE for f1 also evaluates
-- TRUE for f2.
entails :: Form -> Form -> Bool
entails f1 f2 = all (\v -> evl v (Impl f1 f2)) (allVals f1)

-- Function that checks if two formulas are equivalent.
-- Two formulas are equivalent if formula f1 and formula f2
-- have the same outputs for all evaluations.
equiv :: Form -> Form -> Bool
equiv f1 f2 = map (`evl` f1) (allVals f1) == map (`evl` f2) (allVals f2)


prop_validContradiction :: Bool
prop_validContradiction = contradiction $ head $ parse "*(1 -1)"

prop_invalidContradiction :: Bool
prop_invalidContradiction = not (contradiction $ head $ parse "+(1 -1)")

prop_validEquiv :: Bool
prop_validEquiv = f1 `equiv` f1 where f1 = head $ parse "*(1 -1)"

prop_invalidEquiv :: Bool
prop_invalidEquiv = not (f1 `equiv` f2)  where
    f1 = head $ parse "*(1 -1)"
    f2 = head $ parse "+(1 2)"

prop_validTautology :: Bool
prop_validTautology = tautology $ head $ parse "+(1 -1)"

prop_invalidTautology :: Bool
prop_invalidTautology = not (tautology $ head $ parse "*(1 -1)")

prop_validEntailment :: Bool
prop_validEntailment = entails (Prop 1) (Prop 1)

prop_invalidEntailment :: Bool
prop_invalidEntailment = not (entails (Prop 1) (Neg (Prop 1)))


main :: IO ()
main = do
    putStrLn "\n=== Testing Valid Contradiction  ===\n"
    print prop_validContradiction
    putStrLn "\n=== Testing Invalid Contradiction  ===\n"
    print prop_invalidContradiction
    putStrLn "\n=== Test Valid Equivalence ===\n"
    print prop_validEquiv
    putStrLn "\n=== Test Invalid Equivalence ===\n"
    print prop_invalidEquiv
    putStrLn "\n=== Test Valid Tautology ===\n"
    print prop_validTautology
    putStrLn "\n=== Test Invalid Tautology ===\n"
    print prop_invalidTautology
    putStrLn "\n=== Test Valid Entails ===\n"
    print prop_validEntailment
    putStrLn "\n=== Test Invalid Entails ===\n"
    print prop_validEntailment

-- approximation
-- Time spent: 90 minutes --

