{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Exercise5 where
import           Data.Char
import           Data.List
import           Lecture3
import           SetOrd
import           Test.QuickCheck

sub :: Form -> Set Form
sub (  Prop x       ) = Set [Prop x]
sub (  Neg  f       ) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj  [f1, f2]) = unionSet (unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj  [f1, f2]) = unionSet (unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl  f1 f2  ) = unionSet (unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2  ) = unionSet (unionSet (Set [f]) (sub f1)) (sub f2)


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

--quickCheck prop_checkSubContainsCorrectBaseProps
prop_checkSubContainsCorrectBaseProps :: Form -> Bool
prop_checkSubContainsCorrectBaseProps f1= all
    (== True)
    [ inSet baseProp subF1 | baseProp <- baseProps ]  where
    subF1     = sub f1
    baseProps = map Prop (propNames f1)

prop_checkSubContainsIncorrectBaseProps :: Form -> Bool
prop_checkSubContainsIncorrectBaseProps f1= all
    (== True)
    [ inSet baseProp subF1 | baseProp <- baseProps ]  where
    subF1     = sub f1
    baseProps = map Prop (propNames f1)

prop_checkSubContainsFullFormula :: Form -> Bool
prop_checkSubContainsFullFormula f1 = inSet f1 subF1  where
    subF1 = sub f1

prop_checkSubContainsIncorrectFullFormula :: Form -> Bool
prop_checkSubContainsIncorrectFullFormula f1 = inSet (Neg f1) subF1  where
    subF1 = sub f1


-- part 2

nsub :: Form -> Int
nsub f1 = undefined 


