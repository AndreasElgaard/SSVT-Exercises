{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Exercise5 where

import Data.Char
import Data.List
-- import Exercise4 -- Wanted to import Exercise4 so we can use the method to generate formulaes to test on nsub
import Lecture3
import Exercise4
import SetOrd
import Test.QuickCheck

-- Time spend: x minutes --



-- =================================== Functions from Lecture ===================================
sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1, f2]) = unionSet (unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1, f2]) = unionSet (unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet (unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet (unionSet (Set [f]) (sub f1)) (sub f2)

-- =================================== Implementation of Exercise 5.2 ===================================
-- You get the list from sub function, and go over it recursivle by deleting one set a time. Once you delete it
-- you add 1 to the counter.
  

nsub :: Form -> Int
nsub f1 = recursion (sub f1) 0

recursion :: Set Form -> Int -> Int
recursion formSet i
  | isEmpty formSet = 0 
  | otherwise = 1 +  (deleteSet formSet!!i formSet) + (recursion formSet i+1)


-- =================================== Props for Exercise 5.1 ===================================
-- Description of test method here!!

-- Testing if sub contains correct base props
prop_checkSubContainsCorrectBaseProps :: Form -> Bool
prop_checkSubContainsCorrectBaseProps f1 =
  all
    (== True)
    [inSet baseProp subF1 | baseProp <- baseProps]
  where
    subF1     = sub f1
    baseProps = map Prop (propNames f1)

-- Testing if sub contains incorrect base props
prop_checkSubContainsIncorrectBaseProps :: Form -> Bool
prop_checkSubContainsIncorrectBaseProps f1 =
  all
    (== True)
    [inSet baseProp subF1 | baseProp <- baseProps]
  where
    subF1     = sub f1
    baseProps = map Prop (propNames f1)

-- Testing if sub contains full formula
prop_checkSubContainsFullFormula :: Form -> Bool
prop_checkSubContainsFullFormula f1 = inSet f1 subF1
  where
    subF1     = sub f1
    baseProps = map Prop (propNames f1)

-- Testing if sub contains incorrect full formula
prop_checkSubContainsIncorrectFullFormula :: Form -> Bool
prop_checkSubContainsIncorrectFullFormula f1 = inSet (Neg f1) subF1
  where
    subF1     = sub f1
    baseProps = map Prop (propNames f1)

-- =================================== Props for Exercise 5.2 ===================================
-- Properties for nsub
-- 1. Test the lower bound for the nsub of an equation is >= the minimum.
-- It would be a valid test since we know the minimum of each sub-formula (prop = 1, Neg = 2, Cnj = 1, Dsj = 1, Impl = 2, Equiv = 2)
-- I'm not sure how to test this though. But if we find out we can use the generator from Exercise4 to automate the test.


-- =================================== Test Report ===================================
--
main :: IO Result
main = do
  putStrLn "\n=== Testing if sub contains correct base props (Exercise 5.1) ===\n"
  quickCheckResult prop_checkSubContainsCorrectBaseProps

  putStrLn "\n=== Testing if sub contains incorrect base props (Exercise 5.1) ===\n" -- FAILS
  quickCheckResult prop_checkSubContainsIncorrectBaseProps

  putStrLn "\n=== Testing if sub contains full formula (Exercise 5.1) ===\n"
  quickCheckResult prop_checkSubContainsFullFormula

  putStrLn "\n=== Testing if sub contains incorrect full formula (Exercise 5.1) ===\n" -- FAILS
  quickCheckResult prop_checkSubContainsIncorrectFullFormula

-- putStrLn "\n=== Testing if  (Exercise 5.2) ===\n"
-- quickCheck prop_nsub