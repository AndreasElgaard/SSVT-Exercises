module Exercise5 where

import           CFG                            ( EdgeInfo(transitionSource) )
import           GhcPlugins                     ( trueDataCon )
import           LTS
import Exercise4
import           Test.QuickCheck

-- Time spend: X minutes --

-- THIS DOES NOT CONSIDER DELTA! 

-- TASK AT HAND:
-- 

-- =================================== Implementation ===================================

--

out :: IOLTS -> [Label] -> [Label]
out (states, labelsI, labelsO, transitions, init) sigma = 
    noRepetitions
    where 
        noRepetitions = removeDuplicateLabelsTau findingAllLabels sigma
        findingAllLabels = findLabelsForState stateSet transitions
        stateSet = after iolts sigma
        iolts = (states, labelsI, labelsO, transitions, init) 


findLabelsForState :: [State] -> [LabeledTransition] -> [Label]
findLabelsForState _ [] = []
findLabelsForState listStates ((s1, label, s2) : transitions)
    | s1 `elem` listStates = label : findLabelsForState listStates transitions
    | otherwise                       = findLabelsForState listStates transitions

--removes the labels that were in the sigma list (list of labels we want to check) and also 
-- all the tau labels.
removeDuplicateLabelsTau::[Label] -> [Label]-> [Label]
removeDuplicateLabelsTau [] _ = []
removeDuplicateLabelsTau (l1:other) sigma
    | l1 `elem` sigma || l1 == "tau" = removeDuplicateLabelsTau other sigma 
    | otherwise = l1 : removeDuplicateLabelsTau other sigma


-- =================================== Tests ===================================
