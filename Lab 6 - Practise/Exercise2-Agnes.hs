module Exercise2 where

import           Data.List
import           Data.Tuple
import           SetOrd
import           Test.QuickCheck

import Exercise1

-- Time Spents: 120 mins

-- ======================= IMPLEMENTATION ============================

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

-- ======================= Test Properties ============================

-- For union:

prop_elemInResult :: Ord a => Set a -> Set a -> Bool
prop_elemInResult set1 set2 = subSet set1 resultUnion && subSet set2 resultUnion
    where 
    resultUnion = setUnion set1 set2

-- For intersection:

prop_intersection :: Ord a => Set a -> Set a -> Bool
prop_intersection set1 set2 = subSet resultIntersection set1 && subSet resultIntersection set2
    where 
    resultIntersection = setIntersection set1 set2

-- For difference:

prop_difference :: Ord a => Set a -> Set a -> Bool
prop_difference (Set[]) (Set[]) = True
prop_difference (Set[]) set2 = setDifference (Set[]) set2 == set2
prop_difference set1 set2 = subSet intersect1 set1 && (intersect1 == Set[] || not(subSet intersect1 set2))
    where 
    resultDifference = setDifference set1 set2
    intersect1 = setIntersection resultDifference set1

-- ======================= Test Report ============================

main :: IO ()
main = do
    putStrLn "\n=== Testing Union w the generator ===\n"
    quickCheck $ forAll generateSets $ \x-> forAll generateSets $ \y -> prop_elemInResult x y
    putStrLn "\n=== Testing Intersection w the generator ===\n"
    quickCheck $ forAll generateSets $ \x-> forAll generateSets $ \y -> prop_intersection x y
    putStrLn "\n=== Testing Difference w the generator ===\n"
    quickCheck $ forAll generateSets $ \x-> forAll generateSets $ \y -> prop_difference x y
    





-- generate $ generateSets






-- This is what I did, when I didn't realize we had a fucntion which check if a value 
-- is within a set:
-- setIntersection ::   Ord a => Set a -> Set a -> Set a
--setIntersection (Set []) set2 = emptySet
--setIntersection set1 (Set []) = emptySet
--setIntersection (Set (x:xs)) (Set (y:ys)) 
-- | x == y = insertSet x (setIntersection (Set xs) (Set ys))
-- | x > y = setIntersection (Set (x:xs)) (Set ys)
-- | otherwise = setIntersection (Set xs) (Set (y:ys))