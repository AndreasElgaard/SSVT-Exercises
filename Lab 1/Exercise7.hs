{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Exercise7 where
import Test.QuickCheck
-- digit ot int function to ...
import Data.Char ( digitToInt )

parity n = (length n - 2) `mod` 2

innerCheck dig par
    | dig `mod` 2 == par  = dig * 2
    | dig > 9 = dig - 9
    | otherwise = dig

-- copied this from https://paramsingh.dev/blog/b67/ i wanted a piece of code
-- to check whether a digit had more than one digit and then sum them together
--here instead of working with the digits, just specifies all numbers greater that 9
-- which directly means more than one digit. When we have this case, the sum of both digits
-- is the same as doubling the number and then substracting 9. Example: 8*2= 16= 1+6= 7, but 
-- also 8*2 = 16. 16 - 9 = 7
luhnDouble :: Integer -> Integer
luhnDouble n 
    | n*2 > 9 = n*2 - 9
    | otherwise = n*2


sumOddPlaces::[Integer] -> Integer
sumOddPlaces (x:xs) = x 

--sumEvenPlaces::Integer -> Integer
--sumEvenPlaces n = 50 + n


-- https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
digits :: Integer -> [Integer]
digits n = [toInteger (digitToInt x) | x <- show n]

--finalSum::Integer -> Integer
--finalSum = summation digits
-- finalSum n = summation digits n is the same thing

summation::[Integer] -> Integer
summation (x:xs) = luhnDouble x + sumOddPlaces xs
    where length x > 0
    
--luhn ::  Integer -> Bool
--luhn cardNumber
 --   |finalSum cardNumber `mod`10 == 0 = True
   -- | otherwise = False


-- luhn cardNumber = foldr ((+) . innerCheck . read) 0 (show cardNumber)


-- luhn cardNumber = foldr ((+) . innerCheck . read) 0 (show cardNumber)
