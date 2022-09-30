
module Exercise4 where

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
after (states, labelsI, labelsO, transitions, init) sigma 
    | null sigma = [init]
    | null final = []
    | otherwise = final
 where 
    final = tausFinalState ++ last afterImplementation
    tausFinalState = findTaus transitions (last afterImplementation)
    afterImplementation = findAfter [init] sigma transitions
    

-- findTranisitonsTauSet = findTransitionsWithInitState findingFirstSet trans
-- tauSet = findTaus findTransitionsTauSet 

findAfter :: [State] -> [Label] -> [LabeledTransition] -> [[State]]
findAfter _ [] _ = []

findAfter s (l1 : otherLabels) trans
    | null findingFirstTransitionsSet = []
    | not (null  findingFirstSet)  = findingFirstSet : findAfter newState otherLabels trans
    | otherwise = []
  where
    findingFirstTransitionsSet = findTransitionsWithInitState (head s) trans
    findingFirstSet = findFinalStateFromLabel l1 findingFirstTransitionsSet
    newState = findingFirstSet


-- Finds all Labeled Transitions with the initial set we need to check
findTransitionsWithInitState:: State -> [LabeledTransition] -> [LabeledTransition]
findTransitionsWithInitState _ [] = []
findTransitionsWithInitState init ((s1, label, s2) : transitions)
    | init == s1 = (s1, label, s2) : findTransitionsWithInitState init transitions
    | init /= s1 && null transitions = []
    | otherwise = findTransitionsWithInitState init transitions

-- -1 means there is no transitions with that label
findFinalStateFromLabel :: Label -> [LabeledTransition] -> [State]
findFinalStateFromLabel _ [] = []
findFinalStateFromLabel l1 ((s1, label, s2) : transitions)
    | l1 == label = s2 : findFinalStateFromLabel l1 transitions
    | otherwise                       = [] 

    -- finding taus
findTaus :: [LabeledTransition] -> [State]-> [State]
findTaus [] _ = []
findTaus  ((s1, label, s2) : transitions) stat
    | label == "tau" && s1 `elem` stat = s2 : findTaus transitions stat
    | otherwise                       = findTaus transitions stat



-- =================================== Tests ===================================


-- 1: All states that after returns exists in IOLTS list of states
prop_statesExist:: IOLTS -> [Label] -> Bool
prop_statesExist (states, labelsI, labelsO, transitions, init) labels = goThrough resultAfter states
    where iolts = (states, labelsI, labelsO, transitions, init) 
          resultAfter = after iolts labels

-- recursive function to help previous property. 
goThrough:: [State] -> [State]-> Bool
goThrough [] _ = True
goThrough (s1:afterList) ioltsList 
    | s1 `elem` ioltsList = goThrough afterList ioltsList 
    | otherwise = False

-- 2: If the list of labels is not found, it should return empty set
