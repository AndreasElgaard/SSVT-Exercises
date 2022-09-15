module Exercise7 where

import Data.Char
import Test.QuickCheck

-- #### DESCRIPTION OF SOLUTION ####
-- First create structure that holds the length of the IBAN for each country
--

checkLength :: [Char] -> Int -> Bool
checkLength countryCode stringLength = countryList countryCode == fromIntegral stringLength

headToEnd :: [Char] -> [Char]
headToEnd str = drop 4 str ++ take 4 str

replaceLettersWithDigits :: [Char] -> [Char]
replaceLettersWithDigits n = concatMap show (map letterToDigits n)

listToInteger :: [Int] -> Int
listToInteger = read . concatMap show

mod97 :: Integer -> Integer
mod97 n = n `mod` 97

iban :: String -> Bool
iban n = checkLength (take 2 n) (length n) && mod97 (read (replaceLettersWithDigits (headToEnd n))) == 1

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

-- Q: Can you automate the test process?
-- A:

-- Time Spent:
--      ___ hours

countryList :: [Char] -> Integer
countryList "AL" = 28
countryList "AD" = 24
countryList "AT" = 20
countryList "AZ" = 28
countryList "BH" = 22
countryList "BY" = 28
countryList "BE" = 16
countryList "BA" = 20
countryList "BR" = 29
countryList "BG" = 22
countryList "BI" = 27
countryList "CR" = 22
countryList "HR" = 21
countryList "CY" = 28
countryList "CZ" = 24
countryList "DK" = 18
countryList "DO" = 28
countryList "EG" = 29
countryList "SV" = 28
countryList "EE" = 20
countryList "FO" = 18
countryList "FI" = 18
countryList "FR" = 27
countryList "GE" = 22
countryList "DE" = 22
countryList "GI" = 23
countryList "GR" = 27
countryList "GL" = 18
countryList "GT" = 28
countryList "VA" = 22
countryList "HU" = 28
countryList "IS" = 26
countryList "IQ" = 23
countryList "IE" = 22
countryList "IL" = 23
countryList "IT" = 27
countryList "JO" = 30
countryList "KZ" = 20
countryList "XK" = 20
countryList "KW" = 30
countryList "LV" = 21
countryList "LB" = 28
countryList "LY" = 25
countryList "LI" = 21
countryList "LT" = 20
countryList "LU" = 20
countryList "MT" = 31
countryList "MR" = 27
countryList "MU" = 30
countryList "MD" = 24
countryList "MC" = 27
countryList "ME" = 22
countryList "NL" = 18
countryList "MK" = 19
countryList "NO" = 15
countryList "PK" = 24
countryList "PS" = 29
countryList "PL" = 28
countryList "PT" = 25
countryList "QA" = 29
countryList "RO" = 24
countryList "LC" = 32
countryList "SM" = 27
countryList "ST" = 25
countryList "SA" = 24
countryList "RS" = 22
countryList "SC" = 31
countryList "SK" = 24
countryList "SI" = 19
countryList "ES" = 24
countryList "SD" = 18
countryList "SE" = 24
countryList "CH" = 21
countryList "TL" = 23
countryList "TN" = 24
countryList "TR" = 26
countryList "UA" = 29
countryList "AE" = 23
countryList "GB" = 22
countryList "VG" = 24
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

-- replaceCheckDigits :: String -> String
-- replaceCheckDigits n = replaceCharAtIndex 2 '0' (replaceCharAtIndex 3 '0' n)

-- replaceCharAtIndex :: Int -> Char -> String -> String
-- replaceCharAtIndex index replacement str = strHead ++ [replacement] ++ drop 1 strAfter
--   where
--     (strHead, strAfter) = splitAt index str