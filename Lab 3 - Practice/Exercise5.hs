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

prop_checkSubContainsCorrectBaseProps :: Bool
prop_checkSubContainsCorrectBaseProps = all
    (== True)
    [ inSet baseProp subF1 | baseProp <- baseProps ]  where
    subF1     = sub f1
    baseProps = [Prop 1, Prop 2]
    f1        = Neg (Cnj [Prop 1, Prop 2])

prop_checkSubContainsIncorrectBaseProps :: Bool
prop_checkSubContainsIncorrectBaseProps = all
    (== True)
    [ inSet baseProp subF1 | baseProp <- baseProps ]  where
    subF1     = sub f1
    baseProps = [Prop 1, Prop 3]
    f1        = Neg (Cnj [Prop 1, Prop 2])

prop_checkSubContainsFullFormula :: Bool
prop_checkSubContainsFullFormula = inSet f1 subF1  where
    subF1 = sub f1
    f1    = Neg (Cnj [Prop 1, Prop 2])

prop_checkSubContainsIncorrectFullFormula :: Bool
prop_checkSubContainsIncorrectFullFormula = inSet (Neg f1) subF1  where
    subF1 = sub f1
    f1    = Neg (Cnj [Prop 1, Prop 2])


