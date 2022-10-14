module Exercise6 where

import Data.List
import Data.Tuple
import Exercise3
import Exercise5
import SetOrd
import Test.QuickCheck

-- Time Spent: 60 mins

-- =================================== TEST SYMMETRIC CLOSURE ===================================
-- Ref to https://www.geeksforgeeks.org/mathematics-closure-relations-equivalence-relations
-- Closure of Relations section
-- We want to test that our symmetric closure implementatio  in ex 3 works by the following test:

-- TEST: The symmetric closure of relation R on set A is R U R^{-1}, where R^{-1}: inverse of R
-- If such union of R is equivalent to our symClos implementation of Ex 3, we can consider
-- our implementation to pass such a test.

-- Implementation of a Union function on RELATIONS. Similar concept to unionSet provided.
unionRel :: (Ord a) => Rel a -> Rel a -> Rel a
unionRel [] rel2 = sort rel2
unionRel (x : xs) rel2 = sort $ insertRel x (unionRel xs rel2)

insertRel :: (Ord a) => (a, a) -> [(a, a)] -> [(a, a)]
insertRel = insertList

insertList x y
  | x `elem` y = y
  | otherwise = x : y

-- Union of R with R^{-1} -- def. of Symmetric Closure
symCloseUnion :: Ord a => Rel a -> Rel a
symCloseUnion r = unionRel r inverseOfR where inverseOfR = map swap r

-- Property to test equality of our symClos implementation result, with the definition's implementation
-- above.
prop_checkValiditySymClosure :: Ord a => Rel a -> Bool
prop_checkValiditySymClosure r = symClos r == symCloseUnion r

-- To do: Quickcheck implementation

-- =================================== TEST TRANSITIVE CLOSURE ===================================
-- Property that checks that for trClose takes fixed input and returns expected output
prop_trClosReturnExpectedOutput :: Bool
prop_trClosReturnExpectedOutput = trClos inputRelation == trOutput

-- Property that checks if output have any duplicates
prop_trClosHasNoDuplicates :: Bool
prop_trClosHasNoDuplicates = trClos inputRelationDuplicate == nub (trClos inputRelationDuplicate)

-- Property that checks the definiton of transitive closure. The infinite n times of
-- Union of R(i) with R^R(i), until R(n) = R(n-1)
-- Now, we will force a specific relation, so we will have one prop for true and for false.

-- We use a relation which will have true
prop_trClosTrue :: Bool
prop_trClosTrue
  | composeR r r 0 == 1000 = False
  | otherwise = True
  where
    r = inputTrans

-- We choose a relation which will have False.
prop_trClosFalse :: Bool
prop_trClosFalse
  | composeR r r 0 == 1000 = True
  | otherwise = False
  where
    r = inputRelation


composeR :: Eq a => Rel a -> Rel a -> Int -> Int
composeR r1 r2 counter
  | counter == 1000 = 1000
  | counter == 0 = composeR r1 newr2 (counter+1)
  | r1 == r2 = counter
  | otherwise = composeR r1 newr2 (counter+1)
  where newr2 = concatMap (\(x,y) -> concatMap (\(x2, y2) -> [(x,y2) | y == x2]) r1) r2


-- =================================== TEST REPORT ===================================
main :: IO Result
main = do
  putStrLn "\n=== Testing property for transitive closure returns expected output ===\n"
  quickCheckResult prop_trClosReturnExpectedOutput

  putStrLn "\n=== Testing that output of transtive closure does not have any duplicates ===\n"
  quickCheckResult prop_trClosHasNoDuplicates

  putStrLn "\n=== Testing that the transtive closure definition is true ===\n"
  quickCheckResult prop_trClosTrue

  putStrLn "\n=== Testing that the transtive closure definition is false ===\n"
  quickCheckResult prop_trClosFalse

-- =================================== HELPERS ===================================
-- Generator that generates random relations
generateRels :: Gen [(Int, Int)]
generateRels = (arbitrary :: Gen [(Int, Int)])

inputRelation :: [(Integer, Integer)]
inputRelation = [(1, 2), (2, 3), (3, 4)]

inputRelationDuplicate :: [(Integer, Integer)]
inputRelationDuplicate = [(1, 2), (1, 2), (2, 3), (3, 4)]

inputTrans:: [(Integer, Integer)]
inputTrans = [(1,1),(1,4),(2,1),(2,4),(3,1),(3,4)]

trOutput :: [(Integer, Integer)]
trOutput = [(1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4)]
