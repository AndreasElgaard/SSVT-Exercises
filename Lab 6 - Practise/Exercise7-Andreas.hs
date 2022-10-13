module Exercise7 where

import Test.QuickCheck

-- =================================== DISCUSSION ===================================
type Rel a = [(a, a)]

-- Q: Is there a difference between the symmetric closure of the transitive closure of a relation R and the transitive closure of the symmetric closure of R ?
-- A: You can use two approaches to figure out if the symmetric closure of the transitive closure of a relation R is the same.
-- The first one is to find an example where this is the case.
-- The second one is to find a counter-example. If a counter-example is found, the statement would be falsified.
-- To do this we can define a function that checks if the output of symClos and trClos is the same.
-- The input would be an auto generated relation using the autogenerator from exercise7.
prop_checkTrClosEqualSymClos :: Rel Int -> Bool
prop_checkTrClosEqualSymClos relation = not (symClos relation == trClos relation)

-- Test Report
main :: IO Result
main = do
  putStrLn "\n=== Testing if there is a difference between symClos and trClos ===\n"
  quickCheckResult prop_checkTrClosEqualSymClos

-- The quick
