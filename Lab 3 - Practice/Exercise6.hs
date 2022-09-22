{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Exercise6 where

import           Data.Char
import           Data.List
import           GhcPlugins                     ( classDataCon
                                                , xFlags
                                                )
import           Lecture3
import           Test.QuickCheck

type Clause = [Int]
type Clauses = [Clause]

cnf2cls :: Form -> Clauses
cnf2cls (Cnj a) = map (\x -> clauseHandler [x]) a

clauseHandler :: [Form] -> Clause
clauseHandler ((Dsj a) : xs) = clauseHandler a ++ clauseHandler xs
clauseHandler ((Prop a) :                xs) = a : clauseHandler xs
clauseHandler ((        Neg (Prop a) : xs) ) = (-a) : clauseHandler xs
clauseHandler []                             = []
