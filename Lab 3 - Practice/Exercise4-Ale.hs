module Exercise4 where

import           Data.Char
import           Data.List
import           Lecture3
import           SetOrd
import           Test.QuickCheck



instance Arbitrary Form where
    arbitrary = frequency [arbProp, arbNeg, arbCnj, arbDsj aImpl, arbEquiv]
      where
        arbProp = do
            p <- arbitrary
            return $ Prop $ abs p
        arbNeg = do
            x <- frequency [arbProp, arbNeg, arbCnj, arbDsj aImpl, arbEquiv]
            return $ Neg x
        arbitraryCharachter = do
            c <- arbitrary
            return $ Character c









