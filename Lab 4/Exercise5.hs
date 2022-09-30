module Exercise5 where

import           CFG                            ( EdgeInfo(transitionSource) )
import           Exercise4
import           GhcPlugins                     ( trueDataCon )
import           LTS
import           Test.QuickCheck

-- Time spend: 150 minutes --

-- =================================== Implementation ===================================

-- Challenge faced: in the situation where we have delta (quiescence) from one state to another,
-- we are currently not differentiating a scenario like below, where:

-- a= out( k3 after ?but·?but) =out(r1) ∪ out(r4) = {!liq, !choc}
-- b= out( k3 after ?but·δ·?but) =out(r4) ={!choc}

-- Absence of outputs as modelled by quiescence δ is not sufficiently handled. See commented code
-- below as to how we initially tried to manage to handle delta.

-- Our plan was to handle such scenarios, since for such logic, a and b have the same out.

out :: IOLTS -> [Label] -> [Label]
out (states, labelsI, labelsO, transitions, init) sigma = noRepetitions
  where
    noRepetitions    = removeDuplicateLabelsTau findingAllLabels sigma
    findingAllLabels = findLabelsForState stateSet transitions
    -- using the states from the after function defined in ex 4, and getting the labels
    -- achievable from transitions originating from such states
    stateSet         = after iolts sigma
    iolts            = (states, labelsI, labelsO, transitions, init)


findLabelsForState :: [State] -> [LabeledTransition] -> [Label]
findLabelsForState _ [] = []
findLabelsForState listStates ((s1, label, s2) : transitions)
    | s1 `elem` listStates = label : findLabelsForState listStates transitions
    | otherwise            = findLabelsForState listStates transitions

-- Removes the labels that were in the sigma list (list of labels we want to check) and also
-- all the tau labels.
-- This is according to Chapter 4.1 of Tretman's paper.

removeDuplicateLabelsTau :: [Label] -> [Label] -> [Label]
removeDuplicateLabelsTau [] _ = []
removeDuplicateLabelsTau (l1 : other) sigma
    | l1 `elem` sigma || l1 == "tau" = removeDuplicateLabelsTau other sigma
    | otherwise                      = l1 : removeDuplicateLabelsTau other sigma


-- We did not manage to finalise IOCO due to not managing to get all valid straces from an IOLTS in ex.3
-- Our plan was to check if out of all traces in IOLTS X subset out of
-- all traces in IOLTS Y to confirm ioco validity.


-- How we were initially planning to handle quiescent transitions:

-- -- Basically is like the previous functions, but I try that for each state after gets
-- we see if the following label is delta, and only then use a special case where we check
-- if it has other outputs labels. If yes, we do not pass a result that state.
-- --
-- --The recursion does not stop right now, didnt have time to find the bug
-- --
-- findLabelsForState :: [State] -> [LabeledTransition] -> [Label] -> [LabeledTransition]-> [Label]
-- findLabelsForState _ [] _ _= []
-- findLabelsForState _ [] [] _ = []
-- findLabelsForState listStates ((s1, label, s2) : transitions) (former:l1:sigma) fullListTrans
--     | statesIsInList && l1 /= delta = label : findLabelsForState listStates transitions (l1:sigma) fullListTrans
--     | statesIsInList && l1 == delta && checkIfItsLonely = label : findLabelsForState listStates transitions sigma fullListTrans
--     | otherwise                       = findLabelsForState listStates transitions (former:l1:sigma) fullListTrans
--     where
--         statesIsInList = s1 `elem` listStates
--         checkIfItsLonely = 0 == findTransitionsWithState s1 fullListTrans former

-- findTransitionsWithState:: State -> [LabeledTransition] -> Label -> Int
-- findTransitionsWithState _ [] _ = 0
-- findTransitionsWithState init ((s1, label, s2) : transitions) former
--     | init == s1 && label /= former = 1 + findTransitionsWithState init transitions former
--     | init /= s1 && null transitions = 0
--     | otherwise = findTransitionsWithState init transitions former
