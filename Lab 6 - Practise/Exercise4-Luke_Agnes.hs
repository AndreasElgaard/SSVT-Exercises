module Exercise4 where

import           Data.List
import           SetOrd
import           System.Random
import           Test.QuickCheck

type Rel a = [(a, a)]

isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial a [] = True
isSerial a (x : xs) =
  (inSetEq (fst x) (Set domainA) && inSetEq (snd x) (Set domainA))
    && isSerial a xs
  where domainA = nub a


prop_checkRelationValuesSubsetsDomainSet :: Ord a => Set a -> [(a, a)] -> Bool
prop_checkRelationValuesSubsetsDomainSet domain relation = subSet relationSet
                                                                  domain
 where
  relationSet        = list2set relationValueList
  relationValueList  = fst unpackagedRelation ++ snd unpackagedRelation
  unpackagedRelation = unzip relation

prop_checkLength :: Ord a => Set a -> [(a, a)] -> Bool
prop_checkLength (Set domain) relation = lengthDomain >= lengthRelationSet
 where
  lengthDomain       = findLengthSet domain
  lengthRelationSet  = findLengthSet relationSet
  Set relationSet    = list2set relationValueList
  relationValueList  = fst unpackagedRelation ++ snd unpackagedRelation
  unpackagedRelation = unzip relation

findLengthSet []       = 0
findLengthSet (x : xs) = 1 + findLengthSet xs

generateRel :: Gen [(Int, Int)]
generateRel = (arbitrary :: Gen [(Int, Int)]) >>= \x -> return $  sort(nub x)

createSetFromRel :: Gen ([(Int, Int)], Set Int)
createSetFromRel = do
   rels <- generateRel
   let setsFromRel = Set (sort (nub (concatMap (\(x,y) -> [x,y]) rels)))
   return (rels, setsFromRel)

-- genSetAndRels =

-- ======================= Test Report ============================

main :: IO ()
main = do
  putStrLn "\n=== Testing Union w the generator ===\n"
  quickCheck $ forAll createSetFromRel $ \(rels, sets) -> prop_checkRelationValuesSubsetsDomainSet sets rels
    -- C
