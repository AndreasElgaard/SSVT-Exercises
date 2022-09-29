module Exercise1 where

import           CFG                            ( EdgeInfo(transitionSource) )
import           GhcPlugins                     ( trueDataCon )
import           LTS
import           Test.QuickCheck

-- Time spend: X minutes --

--TO DO LIST:
--    - Revise the countable part of the descriptions (two first properties)
--    - Test Report

-- TASK AT HAND:
-- The IOLTS datatype allows, by definition, for the creation of IOLTS's
-- that are not valid. Make a list of factors that result in invalid IOLTS's.

-- Write a function validateLTS :: IOLTS -> Bool that returns true iff a
-- given LTS is valid according to the definition given in the Tretmans paper.

-- Deliverables:
-- list of factors, implementation, short test report, indication of time spent.

-- =================================== List of factors ===================================

--  Def 1. The list of states and list of labels should be countable
--  Def 1. List of states is non empty
--  Def 1. The states within [LabeledTransition] are part of the list of states
--  Def 1. Last value of the IOLTS tuple is part of the list of states
--  Def 1. Tau should not be in the list of Labels.

-- =================================== Implementation ===================================

--
-- Function that validates LTS. Basicalle we added all the proeprties that should be true to validate 
-- the correctnes of the LTS (IOLTS follow the same rules as LTS).
validateLTS :: IOLTS -> Bool
validateLTS iolts = prop_checkStatesAreNonEmpty iolts && prop_checkLabelsAreCountable iolts && prop_checkLabelTransitionStatesAreWithinStatesList iolts && prop_checkInitStateIsSubsetOfStates iolts && prop_checkTauIsNotInLables iolts


-- =================================== Properties ===================================

-- Definition 1 states "Q is a countable, non-empty set of states". Q is the set of states within IOLTS
-- (first value of the tuple), so we checked first if that set is empty or not. For the countable part
-- of the definition ...!!!!!!!!!!!!!!!!!!!
prop_checkStatesAreNonEmpty :: IOLTS -> Bool
prop_checkStatesAreNonEmpty (states, labelsI, labelsO, transitions, init)
  | null states = False
  | otherwise   = True

-- Definition 1 states "L is a countable set of labels;". L is the set of labels within IOLTS
-- (second and third value of the tuple), IF ITS NON EMPTY IT'S STILL COUNTABLE RIGHT?
prop_checkLabelsAreCountable :: IOLTS -> Bool
prop_checkLabelsAreCountable (states, labelsI, labelsO, transitions, init)
  | null labelsI || null labelsO = False
  | otherwise                    = True

-- Definition 1 states "T ⊆ Q". Q being the set of states, and T being the set of Labeled Transitions
-- (fourth value of the tuple). We grabbed the set of Labeled Transitions and, one by one, we grabbed
-- the first and last element of the tuple and check if they were an element of the set of states. Since we 
-- went through a list of tuples [LaveledTransition], we use recursion for that (this is the function "checkLabelTransitionStates")  
prop_checkLabelTransitionStatesAreWithinStatesList :: IOLTS -> Bool
prop_checkLabelTransitionStatesAreWithinStatesList (states, labelsI, labelsO, transitions, init)
  = checkLabelTransitionStates (states, transitions)

-- This is the recursion function that checks for each tuple within the list of LabeledTransition
-- the first and third value of the first tuple is part of the list of states.
checkLabelTransitionStates :: ([State], [LabeledTransition]) -> Bool
checkLabelTransitionStates (states, (s1, label, s2) : transitions) =
  s1 `elem` states && s2 `elem` states && checkLabelTransitionStates
    (states, transitions)
checkLabelTransitionStates (states, []) = True

-- Definition 1 states "τ/∈ L", which means the label tau is not part of the list of labels since 
-- tau is just an internal action that is not supposed to be in the system's environment, thus,
-- should not appear as a label in the list of labels. 
prop_checkTauIsNotInLables :: IOLTS -> Bool
prop_checkTauIsNotInLables (states, labelsI, labelsO, transitions, init)
  | notElem tau labelsI && notElem tau labelsO = True
  | otherwise = False

-- Definition 1 states "q0 ∈ Q is the initial state." q0 is the last value of the tuple, which is 
-- supposed to be the initial state. We check if this value is an element of the list of states.
-- If it is, it returns True, otherwise this is false and the definition not valid.
prop_checkInitStateIsSubsetOfStates :: IOLTS -> Bool
prop_checkInitStateIsSubsetOfStates (states, labelsI, labelsO, transitions, init)
  | init `elem` states = True
  | otherwise = False




-- =================================== Test Report ===================================
--
main :: IO ()
main = do
  putStrLn
    "\n=== Testing if list of states is non empty  ===\n"
  validateLTS
  
