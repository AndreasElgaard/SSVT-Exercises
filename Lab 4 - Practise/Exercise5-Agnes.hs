module Exercise5 where

import           CFG                            ( EdgeInfo(transitionSource) )
import           GhcPlugins                     ( trueDataCon )
import           LTS
import Exercise4
import           Test.QuickCheck

-- Time spend: X minutes --

--TO DO LIST:
--    -

-- TASK AT HAND:
-- 

-- =================================== Implementation ===================================

--

out :: IOLTS -> [Label] -> [Label]
out (states, labelsI, labelsO, transitions, init) sigma = 
    noRepetitions
    where 
        noRepetitions = removeDuplicateLabelsTau findingAllLabels sigma
        findingAllLabels = findLabelsForState stateSet transitions sigma transitions
        stateSet = after iolts sigma
        iolts = (states, labelsI, labelsO, transitions, init) 

-- delta should be in the list of labels
-- i would remove deltas from sigma before after function
-- 
-- 
--
findLabelsForState :: [State] -> [LabeledTransition] -> [Label] -> [LabeledTransition]-> [Label]
findLabelsForState _ [] _ _= []
findLabelsForState _ [] [] _ = []
findLabelsForState listStates ((s1, label, s2) : transitions) (former:l1:sigma) fullListTrans
    | statesIsInList && l1 /= delta = label : findLabelsForState listStates transitions (l1:sigma) fullListTrans
    | statesIsInList && l1 == delta && checkIfItsLonely = label : findLabelsForState listStates transitions sigma fullListTrans
    | otherwise                       = findLabelsForState listStates transitions (former:l1:sigma) fullListTrans
    where
        statesIsInList = s1 `elem` listStates
        checkIfItsLonely = 0 == findTransitionsWithState s1 fullListTrans former

findTransitionsWithState:: State -> [LabeledTransition] -> Label -> Int
findTransitionsWithState _ [] _ = 0
findTransitionsWithState init ((s1, label, s2) : transitions) former
    | init == s1 && label /= former = 1 + findTransitionsWithState init transitions former
    | init /= s1 && null transitions = 0
    | otherwise = findTransitionsWithState init transitions former
--removes the labels that were in the sigma list (list of labels we want to check) and also 
-- all the tau labels.
removeDuplicateLabelsTau::[Label] -> [Label]-> [Label]
removeDuplicateLabelsTau [] _ = []
removeDuplicateLabelsTau (l1:other) sigma
    | l1 `elem` sigma || l1 == "tau" = removeDuplicateLabelsTau other sigma 
    | otherwise = l1 : removeDuplicateLabelsTau other sigma


-- =================================== Tests ===================================


