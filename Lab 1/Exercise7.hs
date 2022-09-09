module Exercise7 where

import Data.Char
import Test.QuickCheck

-- #### DESCRIPTION OF PROCESS ####
-- Take a integer and turn it into a list where every digit have and index 
-- Find out what numbers should be doubled and what should not and create two different lists
-- Create a function that doubles the value of every second digit 
-- If the new number is over 9 subtract 9 from the number
-- Get the sum of all the digits
-- Use the luhn formula to check if the original number is valid and return the bool

--Creates array of digits
digitsToArr :: Integer -> [Integer]
digitsToArr n = [toInteger (digitToInt x) | x <- show n]

-- Creates an array of digits where half of them are doubled
doubledDigitsArr :: [Integer] -> [Integer]
doubledDigitsArr [] = []
doubledDigitsArr (x:y:xs) = luhnDouble x : y :  doubledDigitsArr xs
doubledDigitsArr (x:xs) = luhnDouble x : xs

-- Source: https://paramsingh.dev/blog/b67/ 
-- This function doubles a digit, if the result is above 9 it will subtract 9 from the result.
luhnDouble :: Integer -> Integer
luhnDouble n
    | (n * 2) > 9 = (n * 2) - 9
    | otherwise = n * 2

-- Get the sum of the doubled digits and non-doubled digits
sumOfList :: Integer -> Integer
sumOfList n = sum (doubledDigitsArr (digitsToArr n))

-- This function cheks whether an input number satisfies the Luhn formula.
-- We were a bit confused about the luhn formula since it was described in different ways. 
-- We chose to use the formula found here: https://www.dcode.fr/luhn-algorithm
luhn ::  Integer -> Bool
luhn n = (sumOfList n `mod` 10) == 0

-- Checks whether an input number is a valid American Express Card, Master Card, or Visa Card number.
-- To check if a card is valid properties from this website will be checked: https://yourbusiness.azcentral.com/prepaid-credit-card-work-paypal-22028.html
-- In total three different properties for each card will be cheked:
--    1. The first digit of the number
--    2. The lenght of the number
--    3. Whether or not the number satifies the luhn formula
isAmericanExpress, isMaster, isVisa ::  Integer -> Bool 
isAmericanExpress n = head (digitsToArr n) == 3 && length (digitsToArr n) == 15 && luhn n
isMaster n = head (digitsToArr n) == 5 && length (digitsToArr n) == 16 && luhn n
isVisa n = head (digitsToArr n) == 4 && length (digitsToArr n) == 16 && luhn n

-- Tests
-- Check if a number satifies the luhn formula
prop_inputSatifiesLuhn1 :: Bool
prop_inputSatifiesLuhn1 = luhn 123456789072

-- Check if a number satifies the luhn formula
prop_inputSatifiesLuhn2 :: Bool
prop_inputSatifiesLuhn2 = luhn 123456789247

-- Check if a number satifies the luhn formula
prop_inputSatifiesLuhn3 :: Bool
prop_inputSatifiesLuhn3 = luhn 123456789245

-- Check if a number is of type: American Express
-- This test fails because American Express have an uneven number, which our luhn formula does not support
prop_numberIsAmericanExpress :: Bool
prop_numberIsAmericanExpress = isAmericanExpress 378282246310005

-- Check if a number is of type: MasterCard
prop_numberIsMaster :: Bool
prop_numberIsMaster = isMaster 5431111111111228

-- Check if a number is NOT of type: MasterCard
prop_numberIsNotMaster :: Bool
prop_numberIsNotMaster = not (isMaster 5431111111111225)

-- Check if a number is of type: VisaCard
prop_numberIsVisa :: Bool
prop_numberIsVisa = isVisa 4999999999999996

-- Check if a number is NOT of type: VisaCard
prop_numberIsNotVisa :: Bool
prop_numberIsNotVisa = not (isVisa 3999999999999996)

-- Test Report 
main :: IO Result
main = do
    putStrLn "\n=== Testing if a number satifies the luhn formula ===\n"
    quickCheckResult prop_inputSatifiesLuhn1
    putStrLn "\n=== Testing if a number satifies the luhn formula ===\n"
    quickCheckResult prop_inputSatifiesLuhn2
    putStrLn "\n=== Testing if a number does not satifie the luhn formula ===\n"
    quickCheckResult prop_inputSatifiesLuhn3
    putStrLn "\n=== Testing if an number is a MasterCard ===\n"
    quickCheckResult prop_numberIsMaster
    putStrLn "\n=== Testing if an number is NOT a MasterCard ===\n"
    quickCheckResult prop_numberIsNotMaster
    putStrLn "\n=== Testing if an number is a VisaCard ===\n"
    quickCheckResult prop_numberIsVisa
    putStrLn "\n=== Testing if an number is NOT a VisaCard ===\n"
    quickCheckResult prop_numberIsNotVisa

-- Time Spent:
--      3,5 hours 