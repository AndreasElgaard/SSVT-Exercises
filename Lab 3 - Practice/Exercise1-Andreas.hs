module Exercise1 where

import           Data.Char
import           Data.List
import           Lecture3
import           Test.QuickCheck


contradiction :: Form -> Bool
contradiction = not . satisfiable

tautology :: Form -> Bool
tautology f = all (`evl` f) (allVals f)

-- | logical entailment
entails :: Form -> Form -> Bool
entails f1 f2 = all (\v -> evl v (Impl f1 f2)) (allVals f1)


-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = map (`evl` f1) (allVals f1) == map (`evl` f2) (allVals f2)
