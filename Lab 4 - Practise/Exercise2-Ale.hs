module Exercise2 where

import           Control.Monad.Trans.RWS        ( state )
import           Data.List
import           LTS
import           Test.QuickCheck

genIOLTS :: Gen IOLTS
genIOLTS = do
    states      <- genAllStates 5
    labels      <- genAllLabels 5
    transitions <- genTrans labels states
    return (states, labels, labels, transitions, head states)


-- instance Arbitrary  where
--     arbitrary = return
--         (arbStates, arbLabels, arbLabels, arbTransitions, arbInitState)
--       where
--         arbStates = do
--             genAllStates 5
--         arbLabels = do
--             genAllLabels 5
--         arbInitState = do
--             getInitState arbStates
--         arbTransitions = do
--             genTransitions genAllLabels arbStates

genAllStates :: Int -> Gen [State]
genAllStates vector = vectorOf vector (abs <$> (arbitrary :: Gen Integer))

genAllLabels :: Int -> Gen [Label]
genAllLabels vector = vectorOf vector (return ['A' .. 'Z'])

getInitState :: [State] -> State
getInitState states = do
    head states

-- genTransitions :: [Label] -> [State] -> Gen [LabeledTransition]
-- genTransitions labels states =
--     [LabeledTransition (head states, head labels, last . states)]



-- createTransition start end labels states c stopLoop
--     | c == stopLoop = []
--     | start == end = createTransition start end label states c stopLoop
--     |  otherwise = (start, (elements labels), end) : createTransition end (elements states) labels states (c+1) stopLoop


genTrans :: [State] -> [Label] -> Gen LabeledTransition
genTrans states labels = do
    start <- elements states
    label <- elements labels
    end   <- elements states
    return (start, label, end)



