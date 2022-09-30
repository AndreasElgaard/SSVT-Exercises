module Exercise1 where

import           LTS
import           Test.QuickCheck

-- Time spend: 140 minutes --

--TO DO LIST:
--    - Revise the countable part of the descriptions (two first properties)
--    - Test Report

-- TASK AT HAND:
-- The IOLTS datatype allows, by definition, for the creation of IOLTS's
-- that are not valid. Make a list of factors that result in invalid IOLTS's.

-- Write a function validateLTS :: IOLTS -> Bool that returns true iff a
-- given LTS is valid according to the definition given in the Tretmans paper.

-- Deliverables:
-- list of factors, implementation, short test report, indication of time spent.

-- =================================== List of factors ===================================

--  Def 1. The list of states and list of labels should be countable
--  Def 1. List of states is non empty
--  Def 1. The states within [LabeledTransition] are part of the list of states
--  Def 1. Last value of the IOLTS tuple is part of the list of states
--  Def 1. Tau should not be in the list of Labels.

-- =================================== Implementation ===================================

--
-- Function that validates LTS. Basicalle we added all the properties that should be true to validate
-- the correctnes of the LTS (IOLTS follow the same rules as LTS).
validateLTS :: IOLTS -> Bool
validateLTS iolts =
  prop_checkStatesAreNonEmpty iolts
    && prop_checkLabelsAreCountable iolts
    && prop_checkLabelTransitionStatesAreWithinStatesList iolts
    && prop_checkInitStateIsSubsetOfStates iolts
    && prop_checkTauIsNotInLabels iolts


-- =================================== Properties ===================================

-- Definition 1 states "Q is a countable, non-empty set of states". Q is the set of states within IOLTS
-- (first value of the tuple), so we checked first if that set is empty or not. For the countable part
-- of the definition ...!!!!!!!!!!!!!!!!!!!
prop_checkStatesAreNonEmpty :: IOLTS -> Bool
prop_checkStatesAreNonEmpty (states, labelsI, labelsO, transitions, init)
  | null states = False
  | otherwise   = True

-- Definition 1 states "L is a countable set of labels;". L is the set of labels within IOLTS
-- (second and third value of the tuple), IF ITS NON EMPTY IT'S STILL COUNTABLE RIGHT?
prop_checkLabelsAreCountable :: IOLTS -> Bool
prop_checkLabelsAreCountable (states, labelsI, labelsO, transitions, init)
  | null labelsI || null labelsO = False
  | otherwise                    = True

-- Definition 1 states "T ⊆ Q". Q being the set of states, and T being the set of Labeled Transitions
-- (fourth value of the tuple). We grabbed the set of Labeled Transitions and, one by one, we grabbed
-- the first and last element of the tuple and check if they were an element of the set of states. Since we
-- went through a list of tuples [LabeledTransition], we use recursion for that (this is the function "checkLabelTransitionStates")
prop_checkLabelTransitionStatesAreWithinStatesList :: IOLTS -> Bool
prop_checkLabelTransitionStatesAreWithinStatesList (states, labelsI, labelsO, transitions, init)
  = checkLabelTransitionStates (states, transitions)

-- This is the recursion function that checks for each tuple within the list of LabeledTransition
-- the first and third value of the first tuple is part of the list of states.
checkLabelTransitionStates :: ([State], [LabeledTransition]) -> Bool
checkLabelTransitionStates (states, (s1, label, s2) : transitions) =
  s1 `elem` states && s2 `elem` states && checkLabelTransitionStates
    (states, transitions)
checkLabelTransitionStates (states, []) = True

-- Definition 1 states "τ/∈ L", which means the label tau is not part of the list of labels since
-- tau is just an internal action that is not supposed to be in the system's environment, thus,
-- should not appear as a label in the list of labels.
prop_checkTauIsNotInLabels :: IOLTS -> Bool
prop_checkTauIsNotInLabels (states, labelsI, labelsO, transitions, init)
  | notElem tau labelsI && notElem tau labelsO = True
  | otherwise = False

-- Definition 1 states "q0 ∈ Q is the initial state." q0 is the last value of the tuple, which is
-- supposed to be the initial state. We check if this value is an element of the list of states.
-- If it is, it returns True, otherwise this is false and the definition not valid.
prop_checkInitStateIsSubsetOfStates :: IOLTS -> Bool
prop_checkInitStateIsSubsetOfStates (states, labelsI, labelsO, transitions, init)
  | init `elem` states
  = True
  | otherwise
  = False


-- Testing if parse and form are NOT equal to each other. Here the Impl-Form is being tested
prop_Validate :: Bool
prop_Validate = validateLTS coffeeImpl1


-- =================================== Test Report ===================================
-- The test report tests if all the IOLTS's from LTS.hs (provided by the lecturer) are valid.
main :: IO ()
main = do

  putStrLn "\n=== Testing if coffeeImpl1 is correct ===\n"
  quickCheck (validateLTS coffeeImpl1)

  putStrLn "\n=== Testing if coffeeModel2 is correct ===\n"
  quickCheck (validateLTS coffeeModel2)

  putStrLn "\n=== Testing if coffeeModel3 is correct ===\n"
  quickCheck (validateLTS coffeeModel3)

  putStrLn "\n=== Testing if coffeeModel4 is correct ===\n"
  quickCheck (validateLTS coffeeModel4)

  putStrLn "\n=== Testing if coffeeModel5 is correct ===\n"
  quickCheck (validateLTS coffeeModel5)

  putStrLn "\n=== Testing if coffeeModel6 is correct ===\n"
  quickCheck (validateLTS coffeeModel6)

  putStrLn "\n=== Testing if tretmanK1 is correct ===\n"
  quickCheck (validateLTS tretmanK1)

  putStrLn "\n=== Testing if tretmanK2 is correct ===\n"
  quickCheck (validateLTS tretmanK2)

  putStrLn "\n=== Testing if tretmanK3 is correct ===\n"
  quickCheck (validateLTS tretmanK3)

  putStrLn "\n=== Testing if tretmanI1 is correct ===\n"
  quickCheck (validateLTS tretmanI1)

  putStrLn "\n=== Testing if tretmanI2 is correct ===\n"
  quickCheck (validateLTS tretmanI2)

  putStrLn "\n=== Testing if tretmanI3 is correct ===\n"
  quickCheck (validateLTS tretmanI3)

  putStrLn "\n=== Testing if tretmanI4 is correct ===\n"
  quickCheck (validateLTS tretmanI4)

  putStrLn "\n=== Testing if tretmanS1 is correct ===\n"
  quickCheck (validateLTS tretmanS1)

  putStrLn "\n=== Testing if tretmanS2 is correct ===\n"
  quickCheck (validateLTS tretmanS2)

  putStrLn "\n=== Testing if tretmanS3 is correct ===\n"
  quickCheck (validateLTS tretmanS3)

  putStrLn "\n=== Testing if tretmanS4 is correct ===\n"
  quickCheck (validateLTS tretmanS4)

  putStrLn "\n=== Testing if tretmanR1 is correct ===\n"
  quickCheck (validateLTS tretmanR1)

  putStrLn "\n=== Testing if tretmanR2 is correct ===\n"
  quickCheck (validateLTS tretmanR2)

  putStrLn "\n=== Testing if IOLTS is incorrect ===\n"
  quickCheck
    (not
      (validateLTS
        ( [0, 1, 2, 3, 4, 5]
        , ["but"]
        , ["choc", "liq"]
        , [ (0, "but" , 1)
          , (0, "but" , 2)
          , (1, "liq" , 3)
          , (1, "but" , 1)
          , (2, "but" , 4)
          , (4, "choc", 5)
          , (4, "but" , 4)
          , (5, "but" , 5)
          ]
        , 9
        )
      )
    )


