module Exercise2 where
import           Data.List
import           Data.Tuple
import           SetOrd
import           Test.QuickCheck

setUnion ::  Ord a => Set a -> Set a -> Set a
setUnion = unionSet 


setIntersection ::   Ord a => Set a -> Set a -> Set a
setIntersection (Set []) set2 = emptySet
setIntersection set1 (Set []) = emptySet
setIntersection (Set (x:xs)) set2
    | inSet x set2 = insertSet x (setIntersection (Set xs) set2)
    | otherwise = setIntersection (Set xs) set2
   
-- expl
setDifference ::  Ord a => Set a -> Set a -> Set a
setDifference (Set []) set2 = set2
setDifference set1 (Set []) = emptySet
setDifference (Set (x:xs)) set2 
    | inSet x set2 = setDifference (Set xs) newSet2
    | otherwise = insertSet x (setDifference (Set xs) newSet2)
    where 
        newSet2 = deleteSet x set2

    --[3,4,5] [1,2,3] 
    --[3,4,5][1,3,4]


-- This is what I did, when I didn't realize we had a fucntion which check if a value 
-- is within a set:
-- setIntersection ::   Ord a => Set a -> Set a -> Set a
--setIntersection (Set []) set2 = emptySet
--setIntersection set1 (Set []) = emptySet
--setIntersection (Set (x:xs)) (Set (y:ys)) 
-- | x == y = insertSet x (setIntersection (Set xs) (Set ys))
-- | x > y = setIntersection (Set (x:xs)) (Set ys)
-- | otherwise = setIntersection (Set xs) (Set (y:ys))