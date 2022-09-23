module Exercise3 where

import           Data.Char
import           Data.List
import           Lecture3
import           Test.QuickCheck

-- Applying De Morgan's Law such that all negations apply at atomic level (Prop level).
-- Refer to https://stackoverflow.com/questions/655261/how-to-convert-a-propositional-formula-to-conjunctive-normal-form-cnf
deMorgansHandler :: Form -> Form
deMorgansHandler (Prop x      ) = Prop x
-- By De Morgan's Law: neg (Cnj[a, b]) = Dsj [(neg a), (neg b)]
deMorgansHandler (Neg  (Cnj a)) = Dsj (map (deMorgansHandler . Neg) a)
-- By De Morgan's Law: neg (Dsj[a, b]) = Cnj [(neg a), (neg b)]
deMorgansHandler (Neg  (Dsj a)) = Cnj (map (deMorgansHandler . Neg) a)
deMorgansHandler (Cnj  a      ) = Cnj (map deMorgansHandler a)
deMorgansHandler (Dsj  a      ) = Dsj (map deMorgansHandler a)
-- By De Morgan's Law: neg (Impl (a, b)) = Cnj [a, (neg b)]
deMorgansHandler (Neg (Impl f1 f2)) =
    Cnj [deMorgansHandler f1, Neg (deMorgansHandler f2)]
deMorgansHandler (Impl f1 f2) =
    Impl (deMorgansHandler f1) (deMorgansHandler f2)
deMorgansHandler (Equiv f1 f2) =
    Equiv (deMorgansHandler f1) (deMorgansHandler f2)
deMorgansHandler (Neg (Neg (Prop a))) = Prop a
deMorgansHandler (Neg a             ) = Neg (deMorgansHandler a)

cnf :: Form -> Form
cnf f1 = conjuctiveNormalForm  where
    -- Applying De Morgan's Law to convert the below to CNF form.
    -- Ref. to Q13, SSVT workshop 3
    -- Refer to https://stackoverflow.com/questions/655261/how-to-convert-a-propositional-formula-to-conjunctive-normal-form-cnf
    conjuctiveNormalForm    = deMorgansHandler negatedDisjunctedProps
    -- Disjuncting each combination of props, then applying an outer Negator
    -- Ref. to Q13, SSVT workshop 3
    negatedDisjunctedProps  = Neg (checkDsjForms conjunctedProps)
    -- Conjucting props within each combination
    -- Ref. to Q13, SSVT workshop 3
    conjunctedProps         = map checkCnjProps propsMap
    -- mapping true, false for each prop to have each atomic value as x (true) or -x (false)
    propsMap                = map createFormForProp propsOfFalseEvaluations
    -- Only considering the True, False combinations. Not considering output since we
    -- know output is false for every combination filtered.
    propsOfFalseEvaluations = map fst falseEvaluations
    -- Filtering out all True, False combinations which do not evaluate to False with provided form
    -- Ref. to Q13, SSVT workshop 3
    falseEvaluations = filter (\(_, bool) -> not bool) truthTableEvaluations
    -- Getting evaluations of form for all possible True, False combinations for props
    truthTableEvaluations   = map (\n -> (n, evl n f1)) (allVals f1)

checkCnjProps :: [Form] -> Form
checkCnjProps propsMap | numberOfProps == 1 = head propsMap
                       | otherwise          = Cnj propsMap
    where numberOfProps = length propsMap

checkDsjForms :: [Form] -> Form
checkDsjForms conjunctedProps | numberOfProps == 1 = head conjunctedProps
                              | otherwise          = Dsj conjunctedProps
    where numberOfProps = length conjunctedProps


-- each prop to have each atomic value as x (true) or -x (false)
createFormForProp :: [(Name, Bool)] -> [Form]
createFormForProp ((prop, True ) : xs) = Prop prop : createFormForProp xs
createFormForProp ((prop, False) : xs) = Neg (Prop prop) : createFormForProp xs
createFormForProp []                   = []

-- Time spent: 210 minutes --
