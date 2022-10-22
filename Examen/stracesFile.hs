module Exercise3 where

import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Tuple
import qualified Debug.Trace                   as Debug
import           LTS
import           System.Random
import           Test.QuickCheck

--Time Spent:

third :: (a, b, c) -> c
third (_, _, s) = s

createTransitionMap
  :: [State] -> [LabeledTransition] -> Map State [(Label, State)]
createTransitionMap qs ts =
  Map.fromList [ (q, [ (l, q1) | (q0, l, q1) <- ts, q0 == q ]) | q <- qs ]

insertDelta :: (Label, State) -> [Label] -> [[(Label, State)]]
insertDelta (label, state) li
  | label `elem` li = [[(delta, state), (label, state)], [(label, state)]]
  | otherwise       = [[(label, state)]]

step
  :: [(Label, State)]
  -> Map State [(Label, State)]
  -> [Label]
  -> [[(Label, State)]]
step transitions labelMap li = map
  (\x -> transitions ++ x)
  (concatMap (\x -> insertDelta x li) new_transitions)
 where
  (_, state)      = last transitions
  new_transitions = labelMap Map.! state

extractTrace :: [(Label, State)] -> Trace
extractTrace xs = map (\(label, state) -> label) xs

traces
  :: [Trace]
  -> [[(Label, State)]]
  -> Map State [(Label, State)]
  -> [Label]
  -> [Trace]
traces current [] labelMap li = current
traces current [x] labelMap li =
  current ++ (map extractTrace (step x labelMap li))
traces current (x : xs) labelMap li =
  current ++ traces (map extractTrace new_xs) (xs ++ new_xs) labelMap li
  where new_xs = step x labelMap li

addFirstDelta :: [Label] -> (Label, State) -> [[(Label, State)]]
addFirstDelta li (firstElem, state) =
  if firstElem `elem` li then [[(delta, -1)]] else []

posDelta :: [Label] -> (Label, State) -> [(Label, State)]
posDelta li (cur, state) =
  if cur `elem` li then [(delta, -1), (cur, state)] else [(cur, state)]

insertDeltas :: [Label] -> [[(Label, State)]] -> [[(Label, State)]]
insertDeltas li tr = tr ++ [ concatMap (\x -> posDelta li x) t | t <- tr ]
  where firstDelta = (addFirstDelta li (head $ head tr))

straces :: IOLTS -> [Trace]
straces (qs, li, lu, t, q0) = nub $ allTraces
 where
  start = (map (\(label, state) -> [(label, state)]) (labelMap Map.! q0))
  startTransitions = insertDeltas li start
  startTraces =
    startTransitions ++ (concatMap (\x -> addFirstDelta li (head x)) start)
  allTraces =
    traces (map extractTrace startTraces) startTransitions labelMap li
  labelMap = (createTransitionMap qs t)
