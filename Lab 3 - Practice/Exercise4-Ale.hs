module Exercise4 where

import           Data.Char
import           Data.List
import           Lecture3
import           SetOrd
import           Test.QuickCheck

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
            -- vectorOf
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
            [ (10, arbProp)
            , (2 , arbNeg)
            , (1 , arbCnj)
            , (3 , arbDsj)
            , (4 , arbImpl)
            , (1 , arbEquiv)
            ]


-- genForm :: Gen Form
-- genForm = arbitrary :: Gen Form


prop_isSat :: Form -> Bool
prop_isSat = satisfiable





