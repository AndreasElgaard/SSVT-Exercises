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

-- prop_checkClausesLength:
-- 1. If form submitted has CNJ within outer scope (and has a non-empty
--    inner scope), length of Clauses should be > 1,
--    due to having multiple instance of Clause within Clauses.
-- 2. If form submitted has CNJ within outer scope, but applies to an empty
--    inner scope, length of Clauses should be == 0, no clauses should be present.
-- 3. If form submitted does not have CNJ within outer scope, length of Clauses should be <= 1,
--    due to having, at max, only one instance of a Clause.


prop_checkClausesLength :: Form -> Bool
prop_checkClausesLength f = lengthCheck  where
    lengthCheck = checkClausesLength cnfd
    cnfd        = cnf f

checkClausesLength :: Form -> Bool
checkClausesLength (Cnj []) = null (cnf2cls (Cnj []))
checkClausesLength (Cnj a ) = length (cnf2cls (Cnj a)) > 1
checkClausesLength a        = length (cnf2cls a) <= 1

-- Properties:
-- prop_checkInnerClauseLength:
-- 4. If form submitted contains a DSJ, at least one Clause should have length > 1

prop_checkInnerClauseLength :: Form -> Bool
prop_checkInnerClauseLength f = innerLengthCheck  where
    innerLengthCheck = checkInnerClauseLength cnfd
    cnfd             = cnf f

checkInnerClauseLength :: Form -> Bool
checkInnerClauseLength (Dsj a) = length clauseList
    /= sum (fmap length clauseList)
    where clauseList = cnf2cls (Dsj a)
checkInnerClauseLength (Cnj [Dsj a, b]) = length clauseList
    /= sum (fmap length clauseList)
    where clauseList = cnf2cls (Cnj [Dsj a, b])
checkInnerClauseLength (Cnj [a, Dsj b]) = length clauseList
    /= sum (fmap length clauseList)
    where clauseList = cnf2cls (Cnj [a, Dsj b])
-- this test's purpose is not to check patterns' (other than mentioned above) size of lists of list
checkInnerClauseLength a = True

-- Time spend: 240 minutes --
