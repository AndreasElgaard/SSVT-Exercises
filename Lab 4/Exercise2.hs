module Exercise2 where

import           Data.List
import           Exercise1
import           LTS
import           Test.QuickCheck

-- This function generates an IOLTS
-- Step by step:
--      1. Generates a random integer from 1 to 20, this is used to determine the lenght of the state list, the range in arbitrary,
--          however we decided to only use positive numbers as some functions use negative numbers as a check in later exercises.
--      2. Generate a random integer from 1 to 13, this is used to determine the length of the input labels. The range from 1 to 13 is
--          used because we are using a finite range of Chars to generate the labels for readability, and to do this we use half the alphabet
--          for input and the other half for outputs, hence a range of 13 labels for both input and output. More info in the 'Explanation of
--          label name generation' section
--      3. Same as 2. but for output labels
--      4. Generate the random list of states
--      5. Generate random list of input labels using char range A to N, the output of this generator is a list of labels with a prefix of '?' each
--      6. Same as 5. but with the range of N to Z and with the prefix '!'
--      7. Combines the list of labels together
--      8. Generate an init transition using the states and labels generated.
--      9. Generates all transitions needed for the IOLTS using the initTransition
--          as a starting point, check 'makeAllTransPerms' documentation for more details
--      10. Combines the states, in labels, out labels transitions and an initial state into a four tuple and returns
genIOLTS :: Gen IOLTS
genIOLTS = do
    rNoOfStates    <- chooseInt (1, 10)
    rNoOfInLabels  <- chooseInt (1, 13)
    rNoOfOutLabels <- chooseInt (1, 13)
    states         <- genStates rNoOfStates
    inLabels       <- genLabels rNoOfInLabels '?' 'A' 'M'
    outLabels      <- genLabels rNoOfOutLabels '!' 'N' 'Z'
    let allLabels = inLabels ++ outLabels
    initTransition     <- genTrans states allLabels
    checkedTransitions <- makeAllTransPerms states allLabels [initTransition]
    return (states, inLabels, outLabels, checkedTransitions, head states)

-- Generates a list of unique integers to be used for state generation
genStates :: Int -> Gen [State]
genStates vector = do
    listOfStates <- vectorOf vector (abs <$> (arbitrary :: Gen Integer))
    if checkDupStates listOfStates
        then return listOfStates
        else genStates vector

-- Checks if the list of states is unique, if its not a not, a new list is generated
checkDupStates :: [State] -> Bool
checkDupStates [] = True
checkDupStates (x : xs) | x `elem` xs = False
                        | otherwise   = checkDupStates xs
-- Generates Labels
-- Explanation of label name generation:
--      Rather then creating large random strings that are not readable, I decided to generate random letter
--      based on the range of letters provided. This limits the amount of maximum labels that can be generated
--      but it should still be a sufficient solution. In this implimentation input labels have a range from A to M
--      and output labels have a range of N to Z which sets the maximum unique labels to 13 per. This was done to conserve
--      time as I had already spent over 5 hours on this

-- Paramater explanations:
--      vector -> specifies how long the the list of characters will be
--      ioType -> A character that is prepended to the label, this can be either '!' or '?'
--      startChar -> A character that describes the beginning of the label char range (see the label name generation explanation above)
--      endChar -> A character that describes the end of the label char range (see the label name generation explanation above)

-- This function satisfies the following rules:
--      Generate the Input and Output labels, the labels are unique and the the corresponding symbol is prepended to them (namely ! or ?)
genLabels :: Int -> Char -> Char -> Char -> Gen [Label]
genLabels vector ioType startChar endChar = do
    listOfChars <- vectorOf vector (genChar ioType startChar endChar)
    checkDup (checkDupLabels listOfChars)
             listOfChars
             vector
             ioType
             startChar
             endChar

-- Helper function that if recieves True as the first value will return the passed labels
--      if False it will generate a new set of labels until it is unique
-- Parameter Explanations
--      bool -> Based on the output of checkDupLabels
--      labels -> A list of labels that is checked for duplicates, if True is passed for the bool param
--                this paramter is returned
--      vector -> Same as param from genLabels, only used if False is passed
--      ioType -> Same as param from genLabels, only used if False is passed
--      startChar -> Same as param from genLabels, only used if False is passed
--      endChar -> Same as param from genLabels,only used if False is passed
checkDup :: Bool -> [Label] -> Int -> Char -> Char -> Char -> Gen [Label]
checkDup True labels _vector _ioType _startChar _endChar = do
    return labels
checkDup False _ vector ioType startChar endChar =
    genLabels vector ioType startChar endChar

-- Helper function that checks for duplicate values of Labels, returns True if list if no duplicates are found
--  and False if a duplicate element is found.
checkDupLabels :: [Label] -> Bool
checkDupLabels [] = True
checkDupLabels (x : xs) | x `elem` xs = False
                        | otherwise   = checkDupLabels xs

-- Generates a string with a prefix based on a range of Chars, the explanation for the parameters
--  is explaned in the 'Explanation of label name generation' section above.
genChar :: Char -> Char -> Char -> Gen Label
genChar ioType startChar endChar = do
    randomChar <- elements [startChar .. endChar]
    return (ioType : [randomChar])

-- Generates the initial state of the LTS based on the state list passed
getInitState :: [State] -> State
getInitState states = do
    head states

-- Generates a list of transitions that includes all states at least once.
-- Step by step:
--  1. Checks if each state is included in at least one transition
--      - If True the function will return the list of transitions
--      - If False the function will generate one new transition and prepend it to
--        the transitions list and run the same funciton again
-- NOTE: This is definitely not the most effiecent function, but it satisfies the criteria
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

-- Helper function that checks if a State is in a list of Transitions
isStateInTrans :: State -> [LabeledTransition] -> Bool
isStateInTrans _ [] = False
isStateInTrans s ((s1, l, s2) : xs) | s == s1 || s == s2 = True
                                    | otherwise          = isStateInTrans s xs

-- Generates a transition based on a list of states and labels
-- All attributes are chosen randomly from the lists provided
genTrans :: [State] -> [Label] -> Gen LabeledTransition
genTrans states labels = do
    start <- elements states
    label <- elements labels
    end   <- elements states
    return (start, label, end)

prop_checkIsValid :: IOLTS -> Bool
prop_checkIsValid = validateLTS

main2 :: IO ()
main2 = do
    quickCheck $ forAll genIOLTS prop_checkLabelsAreCountable
    quickCheck $ forAll genIOLTS prop_checkStatesAreNonEmpty
    quickCheck
        $ forAll genIOLTS prop_checkLabelTransitionStatesAreWithinStatesList
    quickCheck $ forAll genIOLTS prop_checkTauIsNotInLabels
    quickCheck $ forAll genIOLTS prop_checkInitStateIsSubsetOfStates

