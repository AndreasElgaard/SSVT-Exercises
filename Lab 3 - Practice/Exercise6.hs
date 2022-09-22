module Exercise6 where

import           Data.Char
import           Data.List
import           Exercise3
import           Lecture3
import           Test.QuickCheck

type Clause = [Int]
type Clauses = [Clause]

tst x = cnf x

