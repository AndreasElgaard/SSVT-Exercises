module Exercise2 where

import           Control.Monad.Trans.RWS        ( state )
import           Data.List
import           Exercise1
import           LTS
import           Test.QuickCheck

genIOLTS :: Gen IOLTS
genIOLTS = do
    states      <- genAllStates 5
    labels      <- genAllLabels 5
    transitions <- vectorOf 5 (genTrans states labels)
    return (states, labels, labels, transitions, head states)

genAllStates :: Int -> Gen [State]
genAllStates vector = vectorOf vector (abs <$> (arbitrary :: Gen Integer))

genAllLabels :: Int -> Gen [Label]
genAllLabels vector = vectorOf vector genChar

genChar :: Gen Label
genChar = do
    a <- elements ['A' .. 'Z']
    return [a]

getInitState :: [State] -> State
getInitState states = do
    head states

genTrans :: [State] -> [Label] -> Gen LabeledTransition
genTrans states labels = do
    start <- elements states
    label <- elements labels
    end   <- elements states
    return (start, label, end)



