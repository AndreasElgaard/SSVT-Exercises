module Exercise7 where

import Exercise3
import Exercise5
import Test.QuickCheck

-- =================================== DISCUSSION ===================================
-- Q: Is there a difference between the symmetric closure of the transitive closure of a relation R and the transitive closure of the symmetric closure of R ?
-- A: You can use two approaches to figure out if the symmetric closure of the transitive closure of a relation R is the same.
-- The first one is to find an example where this is the case.
-- The second one is to find a counter-example. If a counter-example is found, the statement would be falsified.
-- To do this we can define a function that checks if the output of symClos and trClos is the same.
-- The test would only test one relation example: [(1,2),(2,3),(3,4),(4,5)]
type Rel a = [(a, a)]

relation = [(1, 2), (2, 3), (3, 4), (4, 5)]

prop_checkTrClosEqualSymClos :: Rel Integer -> Bool
prop_checkTrClosEqualSymClos = not (trClos (symClos relation) == symClos (trClos relation))

-- =================================== TEST REPORT ===================================
main :: IO Result
main = do
  putStrLn "\n=== Testing if there is a difference between symClos and trClos ===\n"
  quickCheckResult prop_checkTrClosEqualSymClos

-- The the property expects that the two operations does not return the same, and the quickCheck test passes.
-- It can therefore be concluded that it is not true that
-- the symmetric closure of the transitive closure of a relation R and the transitive closure of the symmetric closure of R is the same and that there is a difference between them.