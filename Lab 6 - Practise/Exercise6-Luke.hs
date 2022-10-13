module Exercise6 where
import           Data.List
import           Data.Tuple
import           Exercise3
import           SetOrd
import           Test.QuickCheck

-- Time Spent: 60 mins

-- Ref to https://www.geeksforgeeks.org/mathematics-closure-relations-equivalence-relations
-- Closure of Relations section
-- We want to test that our symmetric closure implementatio  in ex 3 works by the following test:

-- TEST: The symmetric closure of relation R on set A is R U R^{-1}, where R^{-1}: inverse of R
-- If such union of R is equivalent to our symClos implementation of Ex 3, we can consider
-- our implementation to pass such a test.

-- Implementation of a Union function on RELATIONS. Similar concept to unionSet provided.
unionRel :: (Ord a) => Rel a -> Rel a -> Rel a
unionRel []       rel2 = sort rel2
unionRel (x : xs) rel2 = sort $ insertRel x (unionRel xs rel2)

insertRel :: (Ord a) => (a, a) -> [(a, a)] -> [(a, a)]
insertRel = insertList

insertList x y | x `elem` y = y
               | otherwise  = x : y

-- Union of R with R^{-1} -- def. of Symmetric Closure
symCloseUnion :: Ord a => Rel a -> Rel a
symCloseUnion r = unionRel r inverseOfR where inverseOfR = map swap r

-- Property to test equality of our symClos implementation result, with the definition's implementation
-- above.
prop_checkValiditySymClosure :: Ord a => Rel a -> Bool
prop_checkValiditySymClosure r = symClos r == symCloseUnion r

-- To do: Quickcheck implementation
generateRels :: Gen [(Int, Int)]
generateRels = (arbitrary :: Gen [(Int,Int)])
