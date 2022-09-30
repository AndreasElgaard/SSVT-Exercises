module Exercise3 where

import           Exercise2
import           LTS
import           Test.QuickCheck

-- Time Spent : 10+ hours

-- Straces -> The set of all suspension  traces from S

-- Problem Solving
-- The initial idea was to get a list of traces IE lists of labels for every transition pertaining to the process
-- including all possible quiesence transitions (delta) IE this would make it an infinite list of traces.
-- Since we wanted to show all the actual transitions prior to quiesence we were planning to search the transitions
-- in a breadth first approach after which we gather all possible Straces and then invoke all possible combinations
-- of the initially finite as traces with delta at all stages making this an infinite list.

-- Code Planning
-- The plan was to find all transitions originating from the initial state
-- then iterate level by level (breadth first) through the imminent transition while building
-- all the combinations of labels possible to traverse said transitions.

-- Issues found
--  1. Creating a breadth first search proved difficult as through our recursive implimentation we were
--      constantly mananging to only traverse the first child of the first transition from the initial state
--      and drill down further, which is of course depth first.
--  2. When we thought we managed breadth first we were getting many duplicate values for the same transitions
--  3. We didnt manage to add delta randomly to every stage of the transition (to represent quiesence at every state)

search :: [LabeledTransition] -> [LabeledTransition] -> [[LabeledTransition]]
search [] _  = []
search _  [] = []
search transitions ((s1, l, s2) : restOfOccurences) =
    [(s1, l, s2)]
        : filterOnlyStates s2 (tail transitions)
        : search transitions restOfOccurences

filterOnlyStates :: State -> [LabeledTransition] -> [LabeledTransition]
filterOnlyStates s     []               = []
filterOnlyStates state ((s, l, e) : xs) = initFilter  where
    iter        = initFilter ++ filterOnlyStates e notFilter
    notFilter   = filter (\(is, il, ie) -> state /= is) transitions
    initFilter  = filter (\(is, il, ie) -> state == is) transitions
    transitions = (s, l, e) : xs

twoLevelTrace :: [[LabeledTransition]]
twoLevelTrace = search t listOfOccurence
  where
    (_, _, _, t, i) = tretmanK3
    listOfOccurence = filterOnlyStates 0 t

-- prop_genTrace :: IOLTS -> Bool
-- prop_genTrace iolts = not (null (twoLevelTrace iolts))
-- ================== CODE GRAVEYARD IN THE 10 hour + marathon of not getting anywhere ==================================
-- removeUsedTransition :: Int -> [LabeledTransition] -> [LabeledTransition]
-- removeUsedTransition 0     trans = trans

-- removeUsedTransition index trans = tailsO  where
--     (x : xs)        = tailsO
--     (headO, tailsO) = splitAt index trans

-- searchState :: State -> [LabeledTransition] -> [LabeledTransition]
-- searchState state = filter (\(s, l, e) -> s == state)
-- search
--     :: State
--     -> [LabeledTransition]
--     -> [LabeledTransition]
--     -> [LabeledTransition]
-- search _ [] _ = []
-- search s ((s1, l, s2) : xs) listOfOccurences
--     | s == s1 = (s1, l, s2) : getOccurences ((s1, l, s2) : xs) listOfOccurences
--     | otherwise = getOccurences ((s1, l, s2) : xs) listOfOccurences

-- getOccurences
--     :: [LabeledTransition] -> [LabeledTransition] -> [LabeledTransition]
-- getOccurences _ [] = []
-- getOccurences (tHead : tTail) ((x1, l2, x2) : xs) =
--     search x2 tTail (filterOnlyStates x2 tTail)
--         ++ getOccurences (tHead : tTail) xs
-- getOccurences transitions occurences = []

-- filterOnlyStates :: State -> [LabeledTransition] -> [LabeledTransition]
-- filterOnlyStates state = filter (\(s, l, e) -> s == state)

-- test = search i t listOfOccurence
--   where
--     (_, _, _, t, i) = tretmanI3
--     listOfOccurence = filterOnlyStates 0 t

-- mapSearch :: [LabeledTransition] -> [LabeledTransition] -> [LabeledTransition]
-- mapSearch listOfOccurences transitions =
--     getOccurences transitions listOfOccurences

-- straces (states, inPut, outPuts, trans, init) = finalMap
--   where
--     (_, finalMap) = last unprocessed
--     processed     = map (\(l, final) -> l) unprocessed
--     unprocessed   = iterTraces init trans 0 trans

-- iterTraces _ [] counter stored = [("", stored)]
-- iterTraces initState ((s, l, e) : xs) counter stored = output where
--     output =
--         if initState == s then
--             childrenTraces : iterTraces e xs (counter + 1) newList
--         else iterTraces initState xs (counter + 1) stored
--     childrenTraces = iterTraces e newList 0 newList
--     newList = removeUsedTransition counter stored
