module Exercise4 where
import           Exercise1                      ( equiv )
import           Exercise3                      ( cnf )
import           Lecture3
import           Test.QuickCheck
-- Explanation of the Arbitrary
-- Specify each element of the form with a frequency of all of the arbitraries
-- Generating forms within the generator
--    The frequency function is used to generate forms. The function takes a list of tuples as attributes
--    the tuples contain a weight and the arbitrary respectively. The value of the weight determins how often
--    the arbitary will occur in the generation
-- Weight Values explained:
--    Since props are the foundation of the formula they have the highest weight, this makes it more likely
--    that the formula will not be too long, however having too high of a weight will also make props occur
--    too often, leading to a formula consiting of only one prop.
--    The forms which take a list of forms as an attribute (Cnj, Impl) have the lowest weight since they require
--    more forms to be generated, therefore if the weight is too high the formula would be too long and hang the system.
--    The other forms (Neg, Impl, Equiv) have a slightly higher weight compared to the previously mentioned, this is due
--    to them requiring 1 or 2 forms. If the weight is too high the form generation will also hang.
-- VectorOf Usage:`
--     The forms which require a list of forms use vectorOf to generate the list, this was done to improve dynamic generation, however its still limited

-- Future improvements to the generator
--  1. It would be preferable to not al`low (or reduce the occurence) for single prop forms.
--      Better tests would be created with richer forms. We tried to lower the weight of the props
--      but then the form would become too long if a 100 forms are genrated with quickCheck
--  2. The weights of the frequency should be more dynamic, lowering the weight of a form depending on how
--     big the formula current is, however we don't have the knowledge for this  yet
--  3.  The lenght of the list of forms used in vectorOf is currently hard coded to 3, this was done in hopes of
--        not creating never ending forms, however the length should be dynamically generated to have a better solution

-- Reference for this code:
-- https://stackoverflow.com/questions/35726256/quickcheck-on-custom-datatype
-- And our fellow class mate Martin for point us to this source, thank you Martin!
instance Arbitrary Form where
  arbitrary = frequency listOfArbs
   where
    -- The base arbitrary is the prop, which is a literal number, this must be positive and if negative values are allowed
    --    it would count as a negation, this is done through 'abs <$> arbitrary'
    arbProp = do
      Prop . abs <$> arbitrary
    arbNeg = do
      x <- frequency listOfArbs
      return $ Neg x
    arbCnj = do
      x <- vectorOf 3 (frequency listOfArbs)
      return $ Cnj x
    arbDsj = do
      x <- vectorOf 3 (frequency listOfArbs)
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
      [ (18, arbProp)
      , (2 , arbNeg)
      , (1 , arbCnj)
      , (1 , arbDsj)
      , (2 , arbImpl)
      , (2 , arbEquiv)
      ]

-- NOTE: Null checks:
--      The null check is used if the form is not able to be processed to CNF, for example running cnf $ head $ parse "(1==>+(2 1 1))"
--      would return *(), therefor the props would not be the same.

-- Explanation of Prop:
--    Testing that the props of both the attribute form and the CNF'd form are the same.
--    The test handles the case where the attrinbute form cannot be prossed by CNF
prop_cnfHoldsProps :: Form -> Bool
prop_cnfHoldsProps f = null propOfCnf || (propsOfForm == propOfCnf) where
  propsOfForm = propNames f
  propOfCnf   = propNames cnfd
  cnfd        = cnf f

-- Explanation of Prop:
--    Tests if the form and the CNF form are equivalent
prop_isCnfEquiv :: Form -> Bool
prop_isCnfEquiv f = null (propNames cnfd) || cnfd `equiv` f where cnfd = cnf f

-- =================== RELATED TO CNJ TESTING =======================--
-- Checks if a CNF processed form has a CNJ within a CNJ, if so the test fails.
prop_cnjDoesntHaveCnjWithin :: Form -> Bool
prop_cnjDoesntHaveCnjWithin f = cnjCheck where
  cnjCheck = checkLayer1Vals cnfd
  cnfd     = cnf f

checkLayer1Vals :: Form -> Bool
checkLayer1Vals (Prop a   ) = True
checkLayer1Vals (Cnj  []  ) = True -- Pattern match to validate for forms which cannot be CNF'd
checkLayer1Vals (Cnj  a   ) = all checkCnjInCnj a
checkLayer1Vals (Neg  a   ) = checkLayer1Vals a
checkLayer1Vals (Dsj  a   ) = all checkLayer1Vals a
checkLayer1Vals (Impl  a b) = checkLayer1Vals a && checkLayer1Vals b
checkLayer1Vals (Equiv a b) = checkLayer1Vals a && checkLayer1Vals b

checkCnjInCnj :: Form -> Bool
checkCnjInCnj (Prop a   ) = True
checkCnjInCnj (Cnj  a   ) = False
checkCnjInCnj (Neg  a   ) = checkCnjInCnj a
checkCnjInCnj (Dsj  a   ) = all checkCnjInCnj a
checkCnjInCnj (Impl  a b) = checkCnjInCnj a && checkCnjInCnj b
checkCnjInCnj (Equiv a b) = checkCnjInCnj a && checkCnjInCnj b
-- =================== RELATED TO CNJ TESTING =======================--

-- =================== RELATED TO DSJ TESTING =======================--
-- Tests if a DSJ in within a DSJ in the CNF form, if so fail the test.
prop_dnjDoesntHaveDnjWithin :: Form -> Bool
prop_dnjDoesntHaveDnjWithin f = dnjCheck where
  dnjCheck = checkDsjLayer1Vals cnfd
  cnfd     = cnf f

-- The null value of Dsj is not pattern matched in this case because of the following reason:
-- DSJ would only be within the conjucted terms, and if that's present  there need to be props/negated props which are disjuncted in the first place
checkDsjLayer1Vals :: Form -> Bool
checkDsjLayer1Vals (Prop a   ) = True
checkDsjLayer1Vals (Cnj  a   ) = all checkDsjInDsj a
checkDsjLayer1Vals (Neg  a   ) = checkDsjLayer1Vals a
checkDsjLayer1Vals (Dsj  a   ) = all checkDsjLayer1Vals a
checkDsjLayer1Vals (Impl  a b) = checkDsjLayer1Vals a && checkDsjLayer1Vals b
checkDsjLayer1Vals (Equiv a b) = checkDsjLayer1Vals a && checkDsjLayer1Vals b

checkDsjInDsj :: Form -> Bool
checkDsjInDsj (Prop a   ) = True
checkDsjInDsj (Cnj  a   ) = False
checkDsjInDsj (Neg  a   ) = checkDsjInDsj a
checkDsjInDsj (Dsj  a   ) = all checkDsjInDsj a
checkDsjInDsj (Impl  a b) = checkDsjInDsj a && checkDsjInDsj b
checkDsjInDsj (Equiv a b) = checkDsjInDsj a && checkDsjInDsj b

-- =================== RELATED TO DSJ TESTING =======================--

main :: IO ()
main = do
  putStrLn
    "\n=== Testing if the props of the form and of the CNF'd version of the form are equal ===\n"
  quickCheck prop_cnfHoldsProps
  putStrLn "\n=== Testing that no CNJs are within CNJs of the CNF'd form ===\n"
  quickCheck prop_cnjDoesntHaveCnjWithin
  putStrLn "\n=== Testing that no DSJs are within DSJs of the CNF'd form ===\n"
  quickCheck prop_dnjDoesntHaveDnjWithin
  putStrLn
    "\n=== Testing if the CNF form is equivalent to the attribute form ===\n"
  quickCheck prop_isCnfEquiv

-- Extra prop ideas
--  We could have created a prop that tests forms that cannot be processed by CNF, however we do not
--    fully know the rules for this, so for the tests above we handled the case within each prop

-- Test the generator with:
-- generate (arbitrary :: Gen Form)

-- Time spent: Approx 240 minutes, most of the time was spent understanding the QuickCheck generator
