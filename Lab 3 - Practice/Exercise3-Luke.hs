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



cnf :: p -> Form
cnf f1 = deMorgened  where
    -- formula =  Dsj createCnj
    -- createCnj = map Cnj (createFormForProp mapOfListOfProps)
    deMorgened        = deMorgansHandler negatedDisjuncted
    negatedDisjuncted = Neg (Dsj conjuncted)
    conjuncted        = map Cnj propsMap
    propsMap          = map createFormForProp mapOfListOfProps
    mapOfListOfProps  = map fst filteredFalseMap
    filteredFalseMap  = filter (\(val, bool) -> not bool) $ mapOfEval
    mapOfEval         = map (\n -> (n, evl n form2)) (allVals form2)

createFormForProp ((prop, True) : xs) = Prop prop : createFormForProp (xs)
createFormForProp ((prop, False) : xs) =
    Neg (Prop prop) : createFormForProp (xs)
createFormForProp [] = []
