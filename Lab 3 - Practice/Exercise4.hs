module Exercise4 where

import Data.Char
import Data.List
import Lecture3
import SetOrd
import Test.QuickCheck

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

genForm :: IO Form
genForm = generate (arbitrary :: Gen Form)

-- generate (arbitrary :: Gen Form)
prop_isSat :: Form -> Bool
prop_isSat = satisfiable
