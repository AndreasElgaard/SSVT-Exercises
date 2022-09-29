module Exercise2 where

import           Control.Monad.Trans.RWS        ( state )
import           Data.List
import           LTS
import           Test.QuickCheck

genIOLTS :: Gen IOLTS
genIOLTS = do
    states      <- genAllStates 5
    labels      <- genAllLabels 5
    transitiosn <- genTransitions labels states
    return (states, labels, labels, transitiosn, head states)


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

genTransitions :: [Label] -> [State] -> Gen [LabeledTransition]
genTransitions labels states =
    Gen [LabeledTransition (head states, head labels, last . states)]






