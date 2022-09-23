module Exercise3 where

import           Data.Char
import           Data.List
import           Lecture3
import           Test.QuickCheck

cnf :: Form -> Form
cnf = nnf . arrowfree

-- test :: [Form] -> [Form]
-- test (Neg a : Cnj fs       : xs) = Dsj (map Neg fs) : test xs
-- test (Neg a : Dsj fs       : xs) = Cnj (map Neg fs) : test xs
-- test (Neg a : (Impl f1 f2) : xs) = Cnj [f1, Neg f2] : test xs
-- test []                          = []
-- test (x : xs)                    = x : test xs


-- CNJ ([Prop 1, DSJ ([Prop 2, Prop 3])])
-- DSJ(CNJ ([Prop 1, Prop 2]), CNJ ([Prop 1, Prop 3])))

deMorgansHandler :: Form -> Form
deMorgansHandler (Prop x      ) = Prop x
deMorgansHandler (Neg  (Cnj a)) = Dsj (map (Neg . deMorgansHandler) a)
deMorgansHandler (Neg  (Dsj a)) = Cnj (map (Neg . deMorgansHandler) a)
deMorgansHandler (Cnj  a      ) = Cnj (map deMorgansHandler a)
deMorgansHandler (Dsj  a      ) = Dsj (map deMorgansHandler a)
deMorgansHandler (Neg (Impl f1 f2)) =
    Cnj [deMorgansHandler f1, Neg (deMorgansHandler f2)]
deMorgansHandler (Impl f1 f2) =
    Impl (deMorgansHandler f1) (deMorgansHandler f2)
deMorgansHandler (Equiv f1 f2) =
    Equiv (deMorgansHandler f1) (deMorgansHandler f2)
deMorgansHandler x = x

deMorgansHandler1 :: Form -> Form
deMorgansHandler1 (Prop x) = Prop x
deMorgansHandler1 (Neg  f) = Neg (deMorgansHandler1 f)
deMorgansHandler1 (Cnj  a) = Dsj (map (Neg . deMorgansHandler1) a)
deMorgansHandler1 (Dsj  a) = Cnj (map (Neg . deMorgansHandler1) a)
deMorgansHandler1 (Impl f1 f2) =
    Cnj [deMorgansHandler1 f1, Neg (deMorgansHandler1 f2)]
deMorgansHandler1 x = x

-- handler (Cnj a) f1 =

-- distributiveLawHandler :: Form -> Form
-- distributiveLawHandler (Cnj a) = map distributiveLawHandler a
-- distributiveLawHandler (Dsj a) = map



