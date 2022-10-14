module Exercise4 where

import           Data.List
import           SetOrd
import           System.Random
import           Test.QuickCheck

type Rel a = [(a, a)]

-- Assumption: A relation R is serial on a domain A if for ALL x ∈ A there is an y ∈ A such that xRy.
-- This differs from spec in Lab6 in the sense that serial rel implies for ALL x, not for any x.
-- As per: https://en.wikipedia.org/wiki/Serial_relation
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial a [] = True
-- Recursively, we are checking that for every relation in Rel a, both x (1st elem of tuple)
-- and y (2nd elem of tuple) exist within set a. If we have at least one (x, y) in Rel a such that
-- x, or y, are not elements of set a, we can confirm that the relation is NOT serial.
isSerial a (x : xs) =
  (inSetEq (fst x) (Set domainA) && inSetEq (snd x) (Set domainA))
    && isSerial a xs
  where domainA = nub a

-- First Property: we check that all values in relation list form a set which subsets input set domain.
-- The premise is that, since for all relations of type xRy, both x and y are ∈ domain, all values in
-- relation should be in set domain.
prop_checkRelationValuesSubsetsDomainSet :: Ord a => Set a -> [(a, a)] -> Bool
prop_checkRelationValuesSubsetsDomainSet domain relation = subSet relationSet
                                                                  domain
 where
  relationSet        = list2set relationValueList
  relationValueList  = fst unpackagedRelation ++ snd unpackagedRelation
  unpackagedRelation = unzip relation

-- Second Property: We check that, for all distinct values found in relation tuples, the set of such
-- values should have a length less than or equal to the length of set domain.
prop_checkLength :: Ord a => Set a -> [(a, a)] -> Bool
prop_checkLength (Set domain) relation = lengthDomain >= lengthRelationSet
 where
  lengthDomain       = findLengthSet domain
  lengthRelationSet  = findLengthSet relationSet
  Set relationSet    = list2set relationValueList
  relationValueList  = fst unpackagedRelation ++ snd unpackagedRelation
  unpackagedRelation = unzip relation

-- Function created to find the length on set since Set type does not have a default implementation
-- for length
findLengthSet :: Num p => [a] -> p
findLengthSet []       = 0
findLengthSet (x : xs) = 1 + findLengthSet xs

-- Generating a random relation with a random number of tuples (x,y)
generateRel :: Gen [(Int, Int)]
generateRel = (arbitrary :: Gen [(Int, Int)]) >>= \x -> return $ sort (nub x)

-- For test purposes, we want to create a set with all values found in tuples in relation above
-- such that we assure an isSerial relation to test using quickCheck.
createSetFromRel :: Gen ([(Int, Int)], Set Int)
createSetFromRel = do
  rels <- generateRel
  let setsFromRel = Set (sort (nub (concatMap (\(x, y) -> [x, y]) rels)))
  return (rels, setsFromRel)

-- PART C: Consider the relation R = {(x, y) | x = y(mod n)}, where (mod n) is the modulo function
-- in modular arithmetic and n > 0.
    -- Discuss whether (and when) R is serial.
    -- How can you test whether R is serial?
    -- How can you prove that R is serial?

-- The remainder could never be > (n-1), if we perform mod n for any value y.
-- Hence, R is guaranteed to be serial iff:
  -- all y values are element of set A
  -- all values from 0 - (n-1) are contained within set A --> this ensures all possible values of x
  -- are part of set A.

-- We can test to verify whether R is serial by verifying both properties defined above hold for
-- relation R on set A.

-- This can be proved by verifying that, if for n = k, remainder could not be grater than n - 1,
-- then for n+1, it should hold that remainder cannot be greater than n (i.e. k).


-- ======================= Test Report ============================

main :: IO ()
main = do
  putStrLn "\n=== Testing Union w the generator ===\n"
  quickCheck $ forAll createSetFromRel $ \(rels, sets) ->
    prop_checkRelationValuesSubsetsDomainSet sets rels



