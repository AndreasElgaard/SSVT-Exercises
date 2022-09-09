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
luhn ::  Integer -> Bool 
luhn n = (sumOfList n `mod` 10) == 0

-- isAmericanExpress, isMaster, isVisa ::  Integer -> Bool 