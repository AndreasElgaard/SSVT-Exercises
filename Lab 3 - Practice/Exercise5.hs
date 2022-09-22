module Exercise5 where

import Data.Char
import Data.List
-- import Exercise4 -- Wanted to import Exercise4 so we can use the method to generate formulaes to test on nsub
import Lecture3
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
-- This function computes the exact number of sub-formulae of the formula f
-- OPS, I think this calculates the exact number of formulaes, but im not 100 % sure it is what they want,
-- since they say they want a recursive implementation.
nsub :: Form -> Int
nsub (Prop x) = 1
nsub (Neg form) = (nsub form) + 1
nsub (Cnj [form1, form2]) = (nsub form1) + (nsub form2) + 1
nsub (Dsj [form1, form2]) = (nsub form1) + (nsub form2) + 1
nsub (Impl form1 form2) = (nsub form1) + (nsub form2) + 1
nsub (Equiv form1 form2) = (nsub form1) + (nsub form2) + 1

-- =================================== Props for Exercise 5.1 ===================================
-- Description of test method here!!

-- Testing if sub contains correct base props
prop_checkSubContainsCorrectBaseProps :: Bool
prop_checkSubContainsCorrectBaseProps =
  all
    (== True)
    [inSet baseProp subF1 | baseProp <- baseProps]
  where
    subF1 = sub f1
    baseProps = [Prop 1, Prop 2]
    f1 = Neg (Cnj [Prop 1, Prop 2])

-- Testing if sub contains incorrect base props
prop_checkSubContainsIncorrectBaseProps :: Bool
prop_checkSubContainsIncorrectBaseProps =
  all
    (== True)
    [inSet baseProp subF1 | baseProp <- baseProps]
  where
    subF1 = sub f1
    baseProps = [Prop 1, Prop 3]
    f1 = Neg (Cnj [Prop 1, Prop 2])

-- Testing if sub contains full formula
prop_checkSubContainsFullFormula :: Bool
prop_checkSubContainsFullFormula = inSet f1 subF1
  where
    subF1 = sub f1
    f1 = Neg (Cnj [Prop 1, Prop 2])

-- Testing if sub contains incorrect full formula
prop_checkSubContainsIncorrectFullFormula :: Bool
prop_checkSubContainsIncorrectFullFormula = inSet (Neg f1) subF1
  where
    subF1 = sub f1
    f1 = Neg (Cnj [Prop 1, Prop 2])

-- =================================== Props for Exercise 5.2 ===================================
-- Properties for nsub
-- 1. Test the lower bound for the nsub of an equation is >= the minimum.
-- It would be a valid test since we know the minimum of each sub-formula (prop = 1, Neg = 2, Cnj = 1, Dsj = 1, Impl = 2, Equiv = 2)
-- I'm not sure how to test this though. But if we find out we can use the generator from Exercise4 to automate the test.

prop_nsub :: Form -> Bool
prop_nsub = undefined

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