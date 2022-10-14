module Exercise2 where

import Data.List
import Data.Tuple
import Exercise1
import SetOrd
import Test.QuickCheck

-- Time Spents: 120 mins

-- ======================= IMPLEMENTATION ============================
setUnion :: Ord a => Set a -> Set a -> Set a
setUnion = unionSet

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set []) set2 = emptySet
setIntersection set1 (Set []) = emptySet
setIntersection (Set (x : xs)) set2
  | inSet x set2 = insertSet x (setIntersection (Set xs) set2)
  | otherwise = setIntersection (Set xs) set2

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set []) set2 = set2
setDifference set1 (Set []) = emptySet
setDifference (Set (x : xs)) set2
  | inSet x set2 = setDifference (Set xs) newSet2
  | otherwise = insertSet x (setDifference (Set xs) newSet2)
  where
    newSet2 = deleteSet x set2

-- ======================= TEST PROPERTIES ============================
-- Properties for union
prop_elemInResult :: Ord a => Set a -> Set a -> Bool
prop_elemInResult set1 set2 =
  subSet set1 resultUnion && subSet set2 resultUnion
  where
    resultUnion = setUnion set1 set2

prop_unionHasNoDuplicates :: Ord a => Set a -> Set a -> Bool
prop_unionHasNoDuplicates set1 set2 = noDuplicatesInSet (setUnion set1 set2)

-- Properties for intersection:
prop_intersection :: Ord a => Set a -> Set a -> Bool
prop_intersection set1 set2 =
  subSet resultIntersection set1 && subSet resultIntersection set2
  where
    resultIntersection = setIntersection set1 set2

prop_intersectHasNoDuplicates :: Ord a => Set a -> Set a -> Bool
prop_intersectHasNoDuplicates set1 set2 = noDuplicatesInSet (setIntersection set1 set2)

-- Properties for difference:
prop_difference :: Ord a => Set a -> Set a -> Bool
prop_difference (Set []) (Set []) = True
prop_difference (Set []) set2 = setDifference (Set []) set2 == set2
prop_difference set1 set2 =
  subSet intersect1 set1
    && (intersect1 == Set [] || not (subSet intersect1 set2))
  where
    resultDifference = setDifference set1 set2
    intersect1 = setIntersection resultDifference set1

prop_differenceHasNoDuplicates :: Ord a => Set a -> Set a -> Bool
prop_differenceHasNoDuplicates set1 set2 = noDuplicatesInSet (setDifference set1 set2)

-- ======================= Test Report ============================
main :: IO ()
main = do
  putStrLn "\n=== Testing Union with the QuickCheck generator ===\n"
  quickCheck $
    forAll generateSets $ \x ->
      forAll generateSets $ \y -> prop_elemInResult x y

  putStrLn "\n=== Testing Union has no duplicates with the QuickCheck generator ===\n"
  quickCheck $
    forAll generateSets $ \x ->
      forAll generateSets $ \y -> prop_unionHasNoDuplicates x y

  putStrLn "\n=== Testing Intersection with the QuickCheck generator ===\n"
  quickCheck $
    forAll generateSets $ \x ->
      forAll generateSets $ \y -> prop_intersection x y

  putStrLn "\n=== Testing Intersection has no duplicates with the QuickCheck generator ===\n"
  quickCheck $
    forAll generateSets $ \x ->
      forAll generateSets $ \y -> prop_intersectHasNoDuplicates x y

  putStrLn "\n=== Testing Difference with the QuickCheck generator ===\n"
  quickCheck $
    forAll generateSets $ \x ->
      forAll generateSets $ \y -> prop_difference x y

  putStrLn "\n=== Testing Difference has no duplicates with the QuickCheck generator ===\n"
  quickCheck $
    forAll generateSets $ \x ->
      forAll generateSets $ \y -> prop_differenceHasNoDuplicates x y

-- ======================= HELPERS ============================
noDuplicatesInSet :: Ord a => Set a -> Bool
noDuplicatesInSet (Set set) = length set == length (nub set)