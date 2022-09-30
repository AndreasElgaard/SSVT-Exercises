
module Exercise4 where

import           CFG                            ( EdgeInfo(transitionSource) )
import           GhcPlugins                     ( trueDataCon )
import           LTS
import           Test.QuickCheck

-- Time spend: 220 minutes --


-- TASK AT HAND:
-- Implement the function after (infix) for IOLTS corresponding with the definition in the Tretmans paper.
-- Create tests to test the validity of your implementation.

-- Deliverables: Haskell program, tests, short test report, indication of time spent.

-- =================================== Implementation ===================================

-- this is the fucntion after we are asked in the question

-- First, we observe the cases when the given list (sigma) is empty, and return the inital state.
-- then we do the function final which is where the implementation is. 
-- final is the concatenation of all the values after found of the list, and the case if there
-- a tau in the end of a list. We should have had a function like findTaus within the other
-- recursion loops we created, to check taus for every state. We did not have time, so we prefered
-- to only do it in the last state. 
-- afterImplementation is linked to the function findAfter, which is where we do most of the coding,
-- this one returns a list of lists states, such as [2,[3,4]], we only want the last states of the given
-- list sigma, so we only get the last list, by using the last function.

after :: IOLTS -> [Label] -> [State]
after (states, labelsI, labelsO, transitions, init) sigma 
    | null sigma = [init]
    | null final = []
    | otherwise = final
 where 
    final = tausFinalState ++ last afterImplementation
    tausFinalState = findTaus transitions (last afterImplementation)
    afterImplementation = findAfter [init] sigma transitions
    

-- This recursive function goes through all the label list (sigma) and returns the list of lists of 
-- states, we talked about earlier. The algorithm process is the following:
--  1: (findingFirstTransitionSet) We obtain all the transitions that have as the initial
--     state, the state we input as a parameter. In the beginning this will be the initial state
--      of the process, and then the states that have the labels of sigma. 
--  2: Once found all the transitions linked to the state we want, we use (findFinalStateFromLabel)
--      to check if any of these transitions have the label given by sigma
--  3: Once we have these states, we put them in newState variable and redo the findAfter loop
--     until we don-t have any other labels in sigma.

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


-- Finds all Labeled Transitions with the initial set we need to check. Once we check the initial
-- state of a transition is equal to the one we input, we keep this transition and move on with our
-- recursion.
findTransitionsWithInitState:: State -> [LabeledTransition] -> [LabeledTransition]
findTransitionsWithInitState _ [] = []
findTransitionsWithInitState init ((s1, label, s2) : transitions)
    | init == s1 = (s1, label, s2) : findTransitionsWithInitState init transitions
    | init /= s1 && null transitions = []
    | otherwise = findTransitionsWithInitState init transitions

-- We return the final state of every transition that has as a label the one we input in the fucntion.
findFinalStateFromLabel :: Label -> [LabeledTransition] -> [State]
findFinalStateFromLabel _ [] = []
findFinalStateFromLabel l1 ((s1, label, s2) : transitions)
    | l1 == label = s2 : findFinalStateFromLabel l1 transitions
    | otherwise                       = [] 

-- From a list of transitions, finds the ones whose label is tau and their initial state is part 
-- of the found list of states who partake in the trace of the sigma list.
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

