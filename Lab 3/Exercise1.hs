module Exercise1 where

import           Lecture3                       ( Form(Impl, Neg, Prop)
                                                , allVals
                                                , evl
                                                , parse
                                                , satisfiable
                                                )
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
-- Oversight: If this function is based two totally different forms such as (Prop 1) and (Prop 2) it will
--  fail because the values from Prop 1 are not included in Prop 2.
entails :: Form -> Form -> Bool
entails f1 f2 = all (\v -> evl v (Impl f1 f2)) (allVals f1)

-- Function that checks if two formulas are equivalent.
-- Two formulas are equivalent if formula f1 and formula f2
-- have the same outputs for all evaluations.
equiv :: Form -> Form -> Bool
equiv f1 f2 = map (`evl` f1) (allVals f1) == map (`evl` f2) (allVals f2)


-- Verifying that A ^ NOT (A) is a contradiction, as per our contradiction function
-- i.e. A ^ NOT (A) is ALWAYS False
prop_validContradiction :: Bool
prop_validContradiction = contradiction $ head $ parse "*(1 -1)"

-- Verifying that A v NOT (A) is NOT a contradiction, as per our contradiction function
-- i.e. A v NOT (A) is NOT ALWAYS False
prop_invalidContradiction :: Bool
prop_invalidContradiction = not (contradiction $ head $ parse "+(1 -1)")

-- Verifying that A = A is an Equivalence, as per our equivalence function
-- i.e. A = A for ALL true, false combinations
prop_validEquiv :: Bool
prop_validEquiv = f1 `equiv` f1 where f1 = head $ parse "*(1 -1)"

-- Verifying that A != B is an Equivalence, as per our equivalence function
-- i.e. A != B for SOME true, false combinations
prop_invalidEquiv :: Bool
prop_invalidEquiv = not (f1 `equiv` f2)  where
    f1 = head $ parse "*(1 -1)"
    f2 = head $ parse "+(1 2)"

-- Verifying that A v NOT A is a tautology, as per our tautology function
-- i.e. A v NOT A is TRUE for ALL true, false combinations
prop_validTautology :: Bool
prop_validTautology = tautology $ head $ parse "+(1 -1)"

-- Verifying that A ^ NOT A is NOT a tautology, as per our tautology function
-- i.e. A ^ NOT A is NOT TRUE for SOME true, false combinations
prop_invalidTautology :: Bool
prop_invalidTautology = not (tautology $ head $ parse "*(1 -1)")

-- Verifying that A  --> A as per our entailment function
-- i.e. TRUE for A implies TRUE for A
prop_validEntailment :: Bool
prop_validEntailment = entails (Prop 1) (Prop 1)

-- Verifying that A  !--> NOT (A) as per our entailment function
-- i.e. TRUE for A does NOT imply TRUE for NOT (A)
prop_invalidEntailment :: Bool
prop_invalidEntailment = not (entails (Prop 1) (Neg (Prop 1)))

-- note: We tried to perform an entailment test on Prop 1 and Prop 2, but our code does not
--       seem to like it. We are assuming this happend because the allVals for Prop 1 and Prop 2
--       have no equivalence. We think that this can however be improved.


main :: IO ()
main = do
    putStrLn "\n=== Testing Valid Contradiction  ===\n"
    quickCheck prop_validContradiction
    putStrLn "\n=== Testing Invalid Contradiction  ===\n"
    quickCheck prop_invalidContradiction
    putStrLn "\n=== Test Valid Equivalence ===\n"
    quickCheck prop_validEquiv
    putStrLn "\n=== Test Invalid Equivalence ===\n"
    quickCheck prop_invalidEquiv
    putStrLn "\n=== Test Valid Tautology ===\n"
    quickCheck prop_validTautology
    putStrLn "\n=== Test Invalid Tautology ===\n"
    quickCheck prop_invalidTautology
    putStrLn "\n=== Test Valid Entails ===\n"
    quickCheck prop_validEntailment
    putStrLn "\n=== Test Invalid Entails ===\n"
    quickCheck prop_validEntailment

-- approximation
-- Time spent: 90 minutes --

