module Exercise4 where

import Test.QuickCheck

-- #### DESCRIPTION OF PROCESS ####
-- Write a function to identify a prime number
-- Loop through all numbers from 2 to 10000 and save all the prime numbers
-- Reverse all prime numbers and save to a list 
-- Filter all the prime numbers and print to screen

-- First a function is declared that return true if an integer is a prime
isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

-- All prime numbers are found between 2 and 10000 using the filter function
primes :: [Integer]
primes = 2 : filter isPrime [3..10000]

-- A function to reverse an integer
reversal :: Integer -> Integer
reversal = read . reverse . show

-- Reverses all primes in a list
reversedPrimes :: [Integer]
reversedPrimes = map reversal primes

-- Returns all prime numbers, which have a reversal, in a list
reversibleStream :: [Integer]
reversibleStream = filter isPrime reversedPrimes

-- You could also use a list comprehension, the code below:
-- We chose the other design because it splits the functions better
-- A difference is with this way, the list of primes look more organized.

reversibleStreamOtherWay :: [Integer]
reversibleStreamOtherWay = [ x | x <- [1..10000], isPrime x && isPrime(reversal x) ]

-- Q: How would you test this function, by the way?
-- A: The function should be tested to asssure that:
--      All numbers in the list are prime numbers
--      A random element from the list and check if the reversal exists in the list

-- Time Spent:
--      4 hours, since we had to understand how to proprely work with filter map and so on

-- Tests
-- Checks if all numbers in the list are prime numbers
prop_ListConsistOfPrimes :: Bool
prop_ListConsistOfPrimes = all isPrime reversibleStream

-- Check if the reversal of a prime number exists in the list
prop_ReverseInList :: Bool
prop_ReverseInList = all (isReverseInList reversibleStream) reversibleStream

isReverseInList :: [Integer] -> Integer -> Bool
isReverseInList [] _ = False
isReverseInList list first = reversal first `elem` list

-- Test Report 
main :: IO Result
main = do
    putStrLn "\n=== Testing if list consist of only prime numbers ===\n"
    quickCheckResult prop_ListConsistOfPrimes
    putStrLn "\n=== Testing if the reversal of each number exists in the list ===\n"
    quickCheckResult prop_ReverseInList
