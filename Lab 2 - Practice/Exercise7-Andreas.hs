module Exercise6 where

import Data.Char
import Data.Foldable (Foldable (toList))
import Data.List
import Data.Maybe
import Data.Sequence (fromArray, fromList)
import Data.String (IsString (fromString))
import Test.QuickCheck

-- Q: Can you automate the test process?
-- A:

-- #### DESCRIPTION OF SOLUTION ####
-- First create structure that holds the length of the IBAN for each country
--

iban :: String -> Bool
iban n = checkLength (take 2 n) (length n)

checkLength :: [Char] -> Int -> Bool
checkLength countryCode stringLength = countryList countryCode == fromIntegral stringLength

replaceCheckDigits :: String -> String
replaceCheckDigits n = replaceCharAtIndex 2 '0' (replaceCharAtIndex 3 '0' n)

replaceCharAtIndex :: Int -> Char -> String -> String
replaceCharAtIndex index replacement str = strHead ++ [replacement] ++ drop 1 strAfter
  where
    (strHead, strAfter) = splitAt index str

headToEnd :: [Char] -> [Char]
headToEnd str = drop 4 str ++ take 4 str

replaceLettersWithDigits :: [Char] -> Int
replaceLettersWithDigits n = listToInteger (map letterToDigits n)

listToInteger :: [Int] -> Int
listToInteger = read . concatMap show

mod97 n = n `mod` 97

countryList :: [Char] -> Integer
countryList "DK" = 18
countryList "GB" = 20
countryList code = error "unhandlded"

letterToDigits :: Char -> Int
letterToDigits 'A' = 10
letterToDigits 'B' = 11
letterToDigits 'C' = 12
letterToDigits 'D' = 13
letterToDigits 'E' = 14
letterToDigits 'F' = 15
letterToDigits 'G' = 16
letterToDigits 'H' = 17
letterToDigits 'I' = 18
letterToDigits 'J' = 19
letterToDigits 'K' = 20
letterToDigits 'L' = 21
letterToDigits 'M' = 22
letterToDigits 'N' = 23
letterToDigits 'O' = 24
letterToDigits 'P' = 25
letterToDigits 'Q' = 26
letterToDigits 'R' = 27
letterToDigits 'S' = 28
letterToDigits 'T' = 29
letterToDigits 'U' = 30
letterToDigits 'V' = 31
letterToDigits 'W' = 32
letterToDigits 'X' = 33
letterToDigits 'Y' = 34
letterToDigits 'Z' = 35
letterToDigits code = digitToInt code

-- Tests
-- Checks if the length of the list is correct
prop_lengthOfString :: Bool
prop_lengthOfString = undefined

-- Test Report
-- Test Correct and incorrect examples
main :: IO Result
main = do
  putStrLn "\n=== Testing if length of the returned string is equal to 26 ===\n"
  quickCheckResult prop_lengthOfString

-- Time Spent:
--      ___ hours