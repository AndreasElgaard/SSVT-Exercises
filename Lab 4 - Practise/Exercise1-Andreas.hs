module Exercise1 where

import LTS
import Test.QuickCheck

-- Time spend: 180 minutes --

-- =================================== Imlementation ===================================

--
-- Function that validates LTS
validateLTS :: IOLTS -> Bool
validateLTS = undefined

-- =================================== Functions from Lecture ===================================

-- =================================== Properties ===================================
-- Testing if
prop_checkSubContainsCorrectBaseProps :: Int -> Bool
prop_checkSubContainsCorrectBaseProps = undefined

-- Testing if
prop_checkSubContainsFullFormula :: Int -> Bool
prop_checkSubContainsFullFormula f1 = undefined

-- =================================== Test Report ===================================
--
main :: IO ()
main = do
  putStrLn
    "\n=== Testing if sub contains correct base props (Exercise 5.1) ===\n"
  quickCheck prop_checkSubContainsCorrectBaseProps

  putStrLn "\n=== Testing if sub contains full formula (Exercise 5.1) ===\n"
  quickCheck prop_checkSubContainsFullFormula