module Exercise2 where

import Test.QuickCheck
import MultiplicationTable
import Mutation

-- Time spend: x minutes --

-- =================================== DOCUMENTATION OF APPROACH ===================================
-- Write a function that counts the number of survivors and document the effect of which mutations are used 
-- and which properties are used on the number of survivors.

-- =================================== Implementation ===================================
-- The first argument is the number of mutants (4000 in the FitSpec example).
-- The second argument is the list of properties.
-- The third argument is the function under test (the multiplication table function in this case).
-- The output is the number of surviving mutants (0 in the FitSpec example).
countSurvivors :: Integer -> [([Integer] -> Integer -> Property)] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]] -> Integer
countSurvivors = undefined


-- Q: Effect of using different mutators/properties?
-- A: 