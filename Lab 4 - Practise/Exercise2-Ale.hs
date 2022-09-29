module Exercise2 where

import           Data.List
import           Exercise1
import           LTS
import           Test.QuickCheck

genIOLTS :: Gen IOLTS
genIOLTS = do
    states    <- genStates 5
    inLabels  <- genLabels 5 [] '?' 'A' 'M'
    outLabels <- genLabels 5 inLabels '!' 'N' 'Z'
    let allLabels = inLabels ++ outLabels
    initTransitions    <- vectorOf 5 (genTrans states allLabels)
    checkedTransitions <- makeAllTransPerms states allLabels initTransitions
    return (states, inLabels, outLabels, checkedTransitions, head states)

genStates :: Int -> Gen [State]
genStates vector = do
    listOfStates <- vectorOf vector (abs <$> (arbitrary :: Gen Integer))
    if checkDupStates listOfStates
        then return listOfStates
        else genStates vector

checkDupStates :: [State] -> Bool
checkDupStates [] = True
checkDupStates (x : xs) | x `elem` xs = False
                        | otherwise   = checkDupStates xs

genLabels :: Int -> [Label] -> Char -> Char -> Char -> Gen [Label]
genLabels vector currentLabels ioType startChar endChar = do
    listOfChars <- vectorOf vector (genChar ioType startChar endChar)
    checkDup (checkDupLabels listOfChars)
             listOfChars
             vector
             currentLabels
             ioType
             startChar
             endChar

checkDup
    :: Bool -> [Label] -> Int -> [Label] -> Char -> Char -> Char -> Gen [Label]
checkDup True labels _vector _currentLabels _ioType _startChar _endChar = do
    return labels
checkDup False _ vector currentLabels ioType startChar endChar =
    genLabels vector currentLabels ioType startChar endChar

checkDupLabels :: [Label] -> Bool
checkDupLabels [] = True
checkDupLabels (x : xs) | x `elem` xs = False
                        | otherwise   = checkDupLabels xs

genChar :: Char -> Char -> Char -> Gen Label
genChar ioType startChar endChar = do
    randomChar <- elements [startChar .. endChar]
    return (ioType : [randomChar])

getInitState :: [State] -> State
getInitState states = do
    head states

makeAllTransPerms
    :: [State] -> [Label] -> [LabeledTransition] -> Gen [LabeledTransition]
makeAllTransPerms states labels transitions = do
    let isValid = all (`isStateInTrans` transitions) states
    if isValid
        then do
            return transitions
        else do
            newTrans <- vectorOf 1 (genTrans states labels)
            makeAllTransPerms states labels (newTrans ++ transitions)


isStateInTrans :: State -> [LabeledTransition] -> Bool
isStateInTrans _ [] = False
isStateInTrans s ((s1, l, s2) : xs) | s == s1 || s == s2 = True
                                    | otherwise          = isStateInTrans s xs

genTrans :: [State] -> [Label] -> Gen LabeledTransition
genTrans states labels = do
    start <- elements states
    label <- elements labels
    end   <- elements states
    return (start, label, end)
