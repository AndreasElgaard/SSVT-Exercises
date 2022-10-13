module Exercise3 where
import           Data.List
import           Data.Tuple
import           SetOrd
import           Test.QuickCheck

type Rel a = [(a, a)]

-- time spent: 30m

-- Symmetric closure functionality
-- Referred to  below for symmetric closure definition:
--      https://math24.net/closures-relations.html
--      The Haskell Road to Logic, Math and Programming: Ch 5
symClos :: Ord a => Rel a -> Rel a
symClos []       = []
-- we use nub function to eliminate duplicate pairs
-- (inverse of (x, x) is (x,x), hence the inverse is identical)
symClos [x     ] = nub (createSymmetricTuples x)
symClos (x : xs) = nub (createSymmetricTuples x ++ symClos xs)

-- for each tuple within binary relation, we add to our result list the tuple itself,
-- and its inverse (using swap function)
createSymmetricTuples :: (a, a) -> [(a, a)]
createSymmetricTuples a = a : [swap a]

