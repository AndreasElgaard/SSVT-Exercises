module Exercise6 where

import           Data.Char
import           Data.List
import           Exercise3
import           Exercise4
import           GhcPlugins                     ( classDataCon
                                                , xFlags
                                                )
import           Lecture3
import           Test.QuickCheck

type Clause = [Int]
type Clauses = [Clause]

cnf2cls :: Form -> Clauses
cnf2cls (Cnj a) = map (\x -> clauseHandler [x]) a -- separate Clause per conjuncted proposition
cnf2cls a       = [clauseHandler [a]] -- If no outer conjunction, all values contianed within same clause

clauseHandler :: [Form] -> Clause
-- Incomplete Patterns - because of assumption that formulas are already in CNF
-- Ex. Negation of Negation is not possible in CNF, hence no pattern created for such scenario.
clauseHandler ((Dsj a) : xs) = clauseHandler a ++ clauseHandler xs
clauseHandler ((Prop a) :                xs) = a : clauseHandler xs
clauseHandler ((        Neg (Prop a) : xs) ) = (-a) : clauseHandler xs
clauseHandler []                             = []

-- Properties:

-- 1. If form submitted has CNJ within outer scope, length of Clauses should be != 1,
-- due to having multiple instance of Clause within Clauses.
-- 2. If form submitted does not have CNJ within outer scope, length of Clauses should be == 1,
-- due to having only one instance of a Clause.
-- 3. If form submitted contains a DSJ, at least one Clause should have length > 1


