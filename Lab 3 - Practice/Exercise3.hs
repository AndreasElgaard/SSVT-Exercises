module Exercise3 where

import           Data.Char
import           Data.List
import           Lecture3
import           Test.QuickCheck


deMorgansHandler :: Form -> Form
deMorgansHandler (Prop x      ) = Prop x
deMorgansHandler (Neg  (Cnj a)) = Dsj (map (deMorgansHandler . Neg) a)
deMorgansHandler (Neg  (Dsj a)) = Cnj (map (deMorgansHandler . Neg) a)
deMorgansHandler (Cnj  a      ) = Cnj (map deMorgansHandler a)
deMorgansHandler (Dsj  a      ) = Dsj (map deMorgansHandler a)
deMorgansHandler (Neg (Impl f1 f2)) =
    Cnj [deMorgansHandler f1, Neg (deMorgansHandler f2)]
deMorgansHandler (Impl f1 f2) =
    Impl (deMorgansHandler f1) (deMorgansHandler f2)
deMorgansHandler (Equiv f1 f2) =
    Equiv (deMorgansHandler f1) (deMorgansHandler f2)
deMorgansHandler (Neg (Neg (Prop a))) = Prop a
deMorgansHandler (Neg a             ) = Neg (deMorgansHandler a)



cnf :: Form -> Form
cnf f1 = deMorganed  where
    -- formula =  Dsj createCnj
    -- createCnj = map Cnj (createFormForProp mapOfListOfProps)
    deMorganed        = deMorgansHandler negatedDisjuncted
    negatedDisjuncted = Neg (Dsj conjuncted)
    conjuncted        = map Cnj propsMap
    propsMap          = map createFormForProp mapOfListOfProps
    mapOfListOfProps  = map fst filteredFalseMap
    filteredFalseMap  = filter (\(_, bool) -> not bool) mapOfEval
    mapOfEval         = map (\n -> (n, evl n f1)) (allVals f1)

createFormForProp ((prop, True) : xs) = Prop prop : createFormForProp (xs)
createFormForProp ((prop, False) : xs) =
    Neg (Prop prop) : createFormForProp (xs)
createFormForProp [] = []
