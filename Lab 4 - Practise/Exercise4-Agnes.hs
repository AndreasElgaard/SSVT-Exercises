
module Exercise1 where

import           CFG                            ( EdgeInfo(transitionSource) )
import           GhcPlugins                     ( trueDataCon )
import           LTS
import           Test.QuickCheck

-- Time spend: X minutes --

--TO DO LIST:
--    - Document (Agnes)
--    - FindAfter recursion does not finish.
--    - Implement testing

-- TASK AT HAND:
-- Implement the function after (infix) for IOLTS corresponding with the definition in the Tretmans paper.
-- Create tests to test the validity of your implementation.

-- Deliverables: Haskell program, tests, short test report, indication of time spent.

-- =================================== Implementation ===================================

--
after :: IOLTS -> [Label] -> [State]
after (states, labelsI, labelsO, transitions, init) myTrace =
    findAfter init myTrace transitions

-- doesnot finish
findAfter :: State -> [Label] -> [LabeledTransition] -> [State]
findAfter _ [] _ = []
findAfter s (l1 : otherLabels) trans
    | null findingFirstTransitionsSet
    = []
    | findingFirstSet /= -9
    = findingFirstSet : findAfter newState otherLabels trans
    | null otherLabels
    = []
    | otherwise
    = []
  where
    findingFirstTransitionsSet = findTransitionsWithInitState s trans
    findingFirstSet = findFinalStateFromLabel l1 findingFirstTransitionsSet
    newState = findingFirstSet


-- Finds all Labeled Transitions with the initial set we need to check
findTransitionsWithInitState
    :: State -> [LabeledTransition] -> [LabeledTransition]
findTransitionsWithInitState _ [] = []
findTransitionsWithInitState init ((s1, label, s2) : transitions)
    | init == s1 = (s1, label, s2)
    : findTransitionsWithInitState init transitions
    | init /= s1 && null transitions = []
    | otherwise = findTransitionsWithInitState init transitions

-- -9 means there is no transitions with that label
findFinalStateFromLabel :: Label -> [LabeledTransition] -> State
findFinalStateFromLabel _ [] = -9
findFinalStateFromLabel l1 ((s1, label, s2) : transitions)
    | l1 == label                     = s2
    | l1 /= label && null transitions = -9
    | otherwise                       = findFinalStateFromLabel l1 transitions



-- =================================== Tests ===================================

-- 1: All states that after returns exists in IOLTS list of states
-- 2: If we start by init state and go through all the list of labels we are given, we finish
--  in the last state of the list of states "after" returns.
-- 3: All the lists we are given in "after", should be in the list of labels in IOLTS.
