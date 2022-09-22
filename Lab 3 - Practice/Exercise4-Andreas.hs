module Exercise4 where

import Data.Char
import Data.List
import Lecture3
import System.Random
import Test.QuickCheck

-- Time spend: x minutes --

-- =================================== Description of Process ===================================
-- Create a random generator that can selec a number from one to ~10
-- This generator should randomly pick an operation from the Forms.
-- After a random formula have been generated it should be validated.

-- =================================== Implementation ===================================
-- Function that generates a random number
getRandomNum :: IO Int
getRandomNum = randomRIO (1, 10)

--Logic generator

-- Create structure of formula with recursion

-- Function that checks if formula is valied

-- =================================== Props ===================================
-- Testing if parse and form are equal to each other. Form1 is being tested.
prop_isCorrectParse1 :: Bool
prop_isCorrectParse1 = undefined

-- =================================== Test Report ===================================
--
main :: IO Result
main = do
  putStrLn "\n=== Testing if . ===\n"
  quickCheckResult prop_isCorrectParse1