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

-- approximation
-- Time spent: 90 minutes --

