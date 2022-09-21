module Exercise1 where

import           Data.Char
import           Data.List
import           Lecture3
import           Test.QuickCheck


contradiction :: Form -> Bool
contradiction = not . satisfiable

tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

-- | logical entailment
entails :: Form -> Form -> Bool
entails f1 f2 = all checkBools zipped  where
    zipped = zip list1 list2
    list2  = map (\v -> (evl v f2)) (allVals f2)
    list1  = map (\v -> (evl v f1)) (allVals f1)
-- Use Impl for the solution for entails


checkBools :: (Bool, Bool) -> Bool
checkBools (True , True ) = True
checkBools (False, _    ) = True
checkBools (True , False) = False

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 =
    map (\v -> (evl v f1)) (allVals f1) == map (\v -> (evl v f2)) (allVals f2)
