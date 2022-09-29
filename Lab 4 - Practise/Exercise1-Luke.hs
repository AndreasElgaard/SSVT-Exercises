module Exercise1 where

import           CFG                            ( EdgeInfo(transitionSource) )
import           GhcPlugins                     ( trueDataCon )
import           LTS
import           Test.QuickCheck

-- Time spend: X minutes --

-- TASK AT HAND:
-- The IOLTS datatype allows, by definition, for the creation of IOLTS's
-- that are not valid. Make a list of factors that result in invalid IOLTS's.

-- Write a function validateLTS :: IOLTS -> Bool that returns true iff a
-- given LTS is valid according to the definition given in the Tretmans paper.

-- Deliverables:
-- list of factors, implementation, short test report, indication of time spent.

-- =================================== Imlementation ===================================

--
-- Function that validates LTS
validateLTS :: IOLTS -> Bool
validateLTS = undefined

-- =================================== Functions from Lecture ===================================

-- =================================== Properties ===================================
prop_checkStatesAreNonEmpty :: IOLTS -> Bool
prop_checkStatesAreNonEmpty (states, labelsI, labelsO, transitions, init)
  | null states = False
  | otherwise   = True

prop_checkLabelsAreCountable :: IOLTS -> Bool
prop_checkLabelsAreCountable (states, labelsI, labelsO, transitions, init)
  | null labelsI || null labelsO = False
  | otherwise                    = True

prop_checkLabelTransitionStatesAreWithinStatesList :: IOLTS -> Bool
prop_checkLabelTransitionStatesAreWithinStatesList (states, labelsI, labelsO, transitions, init)
  = checkLabelTransitionStates (states, transitions)

checkLabelTransitionStates :: ([State], [LabeledTransition]) -> Bool
checkLabelTransitionStates (states, (s1, label, s2) : transitions) =
  s1 `elem` states && s2 `elem` states && checkLabelTransitionStates
    (states, transitions)
checkLabelTransitionStates (states, []) = True

prop_checkInitStateIsSubsetOfStates :: IOLTS -> Bool
prop_checkInitStateIsSubsetOfStates (states, labelsI, labelsO, transitions, init)
  | init `elem` states
  = True
  | otherwise
  = False

prop_checkTauIsNotInLables :: IOLTS -> Bool
prop_checkTauIsNotInLables (states, labelsI, labelsO, transitions, init)
  | notElem tau labelsI && notElem tau labelsO = True
  | otherwise = False



-- Testing if
prop_checkSubContainsCorrectBaseProps :: Int -> Bool
prop_checkSubContainsCorrectBaseProps = undefined

-- Testing if
prop_checkSubContainsFullFormula :: Int -> Bool
prop_checkSubContainsFullFormula f1 = undefined

-- =================================== Test Report ===================================
--
main :: IO ()
main = do
  putStrLn
    "\n=== Testing if sub contains correct base props (Exercise 5.1) ===\n"
  quickCheck prop_checkSubContainsCorrectBaseProps

  putStrLn "\n=== Testing if sub contains full formula (Exercise 5.1) ===\n"
  quickCheck prop_checkSubContainsFullFormula
