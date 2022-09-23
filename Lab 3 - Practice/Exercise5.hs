module Exercise5 where

import Data.Char
import Data.List
-- import Exercise4 -- Wanted to import Exercise4 so we can use the method to generate formulaes to test on nsub
import Lecture3
import Exercise4
import SetOrd
import Test.QuickCheck

-- Time spend: x minutes --

-- i copied this from exercise 4, the import did not work, so I just copied it. 
instance Arbitrary Form where
  arbitrary = frequency listOfArbs
    where
      arbProp = do
        Prop . abs <$> arbitrary
      arbNeg = do
        x <- frequency listOfArbs
        return $ Neg x
      arbCnj = do
        x <- vectorOf 4 (frequency listOfArbs)
        return $ Cnj x
      arbDsj = do
        x <- vectorOf 4 (frequency listOfArbs)
        return $ Dsj x
      arbImpl = do
        x <- frequency listOfArbs
        y <- frequency listOfArbs
        return $ Impl x y
      arbEquiv = do
        x <- frequency listOfArbs
        y <- frequency listOfArbs
        return $ Equiv x y
      listOfArbs =
        [ (13, arbProp),
          (2, arbNeg),
          (2, arbCnj),
          (2, arbDsj),
          (2, arbImpl),
          (2, arbEquiv)
        ]

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

prop_nsub :: Form -> Bool
prop_nsub = undefined

-- =================================== Test Report ===================================
--
main :: IO Result
main = do
  putStrLn "\n=== Testing if sub contains correct base props (Exercise 5.1) ===\n"
  quickCheck prop_checkSubContainsCorrectBaseProps

  putStrLn "\n=== Testing if sub contains incorrect base props (Exercise 5.1) ===\n" -- FAILS
  quickCheck prop_checkSubContainsIncorrectBaseProps

  putStrLn "\n=== Testing if sub contains full formula (Exercise 5.1) ===\n"
  quickCheck prop_checkSubContainsFullFormula

  putStrLn "\n=== Testing if sub contains incorrect full formula (Exercise 5.1) ===\n" -- FAILS
  quickCheck prop_checkSubContainsIncorrectFullFormula

-- putStrLn "\n=== Testing if  (Exercise 5.2) ===\n"
-- quickCheck prop_nsub