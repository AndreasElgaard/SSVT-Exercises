module Exercise1 where

import Data.Char
import Data.List
import Lecture3
import Test.QuickCheck

-- =================================== Implementation ===================================
-- Function that checks if a formula is a contradiction.
-- A formula is a contradiction if is not satisfied for any values
contradiction :: Form -> Bool
contradiction = not . satisfiable

-- Function that checks if a formula is a tautology.
-- A formula is a tautology if all values are satisfied.
tautology :: Form -> Bool
tautology f = all (`evl` f) (allVals f)

-- Function that checks if a formula f1 entails formula f2 if and only if every truth assignment that satisfies f1 also satisfies f2.
entails :: Form -> Form -> Bool
entails f1 f2 = all (\v -> evl v (Impl f1 f2)) (allVals f1)

-- Function that checks if two formulas are equivalent.
-- Two formulas are equivalent if formula f1 and formula f2 have the same outputs for all valuations.
equiv :: Form -> Form -> Bool
equiv f1 f2 = map (`evl` f1) (allVals f1) == map (`evl` f2) (allVals f2)
