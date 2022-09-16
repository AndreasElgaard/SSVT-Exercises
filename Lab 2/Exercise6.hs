module Exercise6 where

import Data.Maybe
import Test.QuickCheck


-- Time Spent:
--      3 hours

-- Q: Give a specification of ROT13.
-- A: ROT13 is a substitution cipher that replaces a letter with a letter 13 letters after it in the (english) alphabet.
-- This means that it is its own inverse, which means to decrypt ROT13, the same algorithm is applied and the original value will be visible.

-- #### DESCRIPTION OF SOLUTION ####
-- First of all should we define a function that replaces each letter with the letter 13 places further along in the (english) alphabet.
-- This function should be applied on all letters in the list and return a new list. To the this we should use map

-- Iterates over all characters in the list of characters to find the inverse.
-- Returns a list consisting of the inverse characters
rot13 :: [Char] -> [Char]
rot13 = map rot13OnChar

-- This function creates a case of where it uses the lookup function, provided by GHC.List, to find the key in a list.
-- On line 22 it first searches for uppercase characters in the english alphabet.
-- If it finds the key it will return the corresponding value seen on line 21.
-- If it does not find a key that matches, it will use the fromMaybe function to search for lowercase characters in the english alphabet.
rot13OnChar :: Char -> Char
rot13OnChar char = case lookup char (rot13Pairs lowercaseAlphabet) of
  Just inverseChar -> inverseChar
  Nothing -> fromMaybe char (lookup char (rot13Pairs uppercaseAlphabet))

-- This function creates a list of tuples that consists of the rot13 pairs i.e. [('A','N')..('Z','M')]
-- To create the rot13 pairs it uses the zip function to concatenate two lists and returns a list of corresponding pairs
-- The first list is the original list consisting of the entire english alphabet in lowercase/uppercase letters.
-- The second list is the inverse of the original i.e. "NOPQRSTUVWXYZABCDEFGHIJKLM". To do this it take the last 13 characters in the alphabet and
-- the first 13 characters in the alphabet and appends the two list together i.e. the first 13 characters in the alphabet will not be the last 13 characters in the string.
rot13Pairs :: [Char] -> [(Char, Char)]
rot13Pairs strAlphabet = zip strAlphabet listPairs
  where
    listPairs = firstHalf ++ secondHalf
    firstHalf = drop 13 strAlphabet
    secondHalf = take 13 strAlphabet

-- A function that returns a list consisting of lowercase letters in the english alphabet
lowercaseAlphabet :: [Char]
lowercaseAlphabet = ['a' .. 'z']

-- A function that returns a list consisting of uppercase letters in the english alphabet
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
prop_applyRot13Twice :: [Char] -> Bool
prop_applyRot13Twice str = rot13 (rot13 str) == str

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
