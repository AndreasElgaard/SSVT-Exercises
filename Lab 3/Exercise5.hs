module Exercise5 where
import           Lecture3

import           SetOrd
import           Test.QuickCheck

-- Time spend: 120 minutes --

instance Arbitrary Form where
  arbitrary = frequency listOfArbs
   where
    arbProp = do
      Prop . abs <$> arbitrary
    arbNeg = do
      x <- frequency listOfArbs
      return $ Neg x
    arbCnj = do
      x <- frequency listOfArbs
      y <- frequency listOfArbs
      return $ Cnj [x, y]
    arbDsj = do
      x <- frequency listOfArbs
      y <- frequency listOfArbs
      return $ Dsj [x, y]
    arbImpl = do
      x <- frequency listOfArbs
      y <- frequency listOfArbs
      return $ Impl x y
    arbEquiv = do
      x <- frequency listOfArbs
      y <- frequency listOfArbs
      return $ Equiv x y
    listOfArbs =
      [ (20, arbProp)
      , (2 , arbNeg)
      , (1 , arbCnj)
      , (1 , arbDsj)
      , (2 , arbImpl)
      , (2 , arbEquiv)
      ]

-- =================================== Functions from Lecture ===================================
sub :: Form -> Set Form
sub (  Prop x       ) = Set [Prop x]
sub (  Neg  f       ) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj  [f1, f2]) = unionSet (unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj  [f1, f2]) = unionSet (unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl  f1 f2  ) = unionSet (unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2  ) = unionSet (unionSet (Set [f]) (sub f1)) (sub f2)

-- =================================== Implementation of Exercise 5.2 ===================================
-- You get the list from sub function, and go over it recursively by using a smaller set every iteration. Every
-- iteration increments the value of sets. The recursion stops at set xs which means we are in the last iteration, we output 1 as
-- well to count the last subset.

nsub :: Form -> Int
nsub f1 = countSubs (sub f1)

countSubs :: Set Form -> Int
countSubs (Set []      ) = 0
countSubs (Set (x : xs)) = 1 + countSubs (Set xs)

-- =================================== Props for Exercise 5.1 ===================================
-- To test the sub function, we will use the generator we used in the
-- previous exercise (Exercise4).  First of all, we want to check that for any form we generate
-- the props in it are in the set delivered by the function sub. Afterwards, we check the whole formula
-- appears in the sub functions.
-- Basically we check for:
--      1. The right props in sub using a random generated formula
--      2. The whole formula in sub using a random formula.

-- Testing if sub contains correct base props
prop_checkSubContainsCorrectBaseProps :: Form -> Bool
prop_checkSubContainsCorrectBaseProps f1 = all
  (== True)
  [ inSet baseProp subF1 | baseProp <- baseProps ]
 where
  subF1     = sub f1
  baseProps = map Prop (propNames f1)

-- Testing if sub contains full formula
prop_checkSubContainsFullFormula :: Form -> Bool
prop_checkSubContainsFullFormula f1 = inSet f1 subF1
 where
  subF1     = sub f1
  baseProps = map Prop (propNames f1)


-- =================================== Props for Exercise 5.2 ===================================
-- Properties for nsub
-- 1. Test the lower bound for the nsub of an equation is >= the minimum.
-- It would be a valid test since we know the minimum of each sub-formula (prop = 1, Neg = 2, Cnj = 1, Dsj = 1, Impl = 2, Equiv = 2)
-- I'm not sure how to test this though. But if we find out we can use the generator from Exercise4 to automate the test.

-- we have more sets in nsub than props there are in the form
prop_nsub :: Form -> Bool
prop_nsub f1 = nsub f1 >= length (map Prop (propNames f1))

-- =================================== Test Report ===================================
--
main :: IO ()
main = do
  putStrLn
    "\n=== Testing if sub contains correct base props (Exercise 5.1) ===\n"
  quickCheck prop_checkSubContainsCorrectBaseProps


  putStrLn "\n=== Testing if sub contains full formula (Exercise 5.1) ===\n"
  quickCheck prop_checkSubContainsFullFormula


  putStrLn "\n=== Testing if  (Exercise 5.2) ===\n"
  quickCheck prop_nsub
