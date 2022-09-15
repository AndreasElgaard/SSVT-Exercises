module Exercise6 where

import Data.Char
import Data.List
import Data.Maybe
import Test.QuickCheck

-- Q: Give a specification of ROT13.
-- A:

-- #### DESCRIPTION OF SOLUTION ####
-- First of all should we define a function that replaces each letter with the letter 13 places further along in the (english) alphabet.
-- This function should be applied on all letters in the list and return a new list

rot13 :: [Char] -> [Char]
rot13 = map rot13OnChar

rot13OnChar :: Char -> Char
rot13OnChar char = case lookup char (rot13Pairs lowercaseAlphabet) of
  Just inverseChar -> inverseChar
  Nothing -> fromMaybe char (lookup char (rot13Pairs uppercaseAlphabet))

rot13Pairs :: [Char] -> [(Char, Char)]
rot13Pairs n = zip n (drop 13 n ++ take 13 n)

lowercaseAlphabet :: [Char]
lowercaseAlphabet = ['a' .. 'z']

uppercaseAlphabet :: [Char]
uppercaseAlphabet = ['A' .. 'Z']

-- Tests
-- Checks if the length of the list is correct
prop_lengthOfString :: Bool
prop_lengthOfString = length (rot13 uppercaseAlphabet) == 26

-- Check if the inverse of a character exists in the list for uppercase letters
prop_InverseUppercaseInList :: Bool
prop_InverseUppercaseInList = all (isInverseInList uppercaseAlphabet) (rot13 ['A', 'N', 'Z'])

-- Check if the inverse of a character exists in the list for lowercase letters
prop_InverseLowercaseInList :: Bool
prop_InverseLowercaseInList = all (isInverseInList lowercaseAlphabet) (rot13 ['a', 'n', 'z'])

isInverseInList :: [Char] -> Char -> Bool
isInverseInList [] _ = False
isInverseInList list first = rot13OnChar first `elem` list

-- Apply rot13 twice to have the original value
prop_applyRot13Twice :: Bool
prop_applyRot13Twice = rot13 (rot13 ['A', 'n', 'q', 'Z']) == "AnqZ"

-- Test Report
main :: IO Result
main = do
  putStrLn "\n=== Testing if length of the returned string is equal to 26 ===\n"
  quickCheckResult prop_lengthOfString

  putStrLn "\n=== Testing if the inverse of a character exists in the list for lowercase letters===\n"
  quickCheckResult prop_InverseLowercaseInList

  putStrLn "\n=== Testing if the inverse of a character exists in the list for uppercase letters ===\n"
  quickCheckResult prop_InverseUppercaseInList

  putStrLn "\n=== Testing if ROT13 is applied twice the original value will be returned ===\n"
  quickCheckResult prop_applyRot13Twice

-- Time Spent:
--      3 hours