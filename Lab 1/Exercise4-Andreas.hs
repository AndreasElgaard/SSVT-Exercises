module Exercise4 where
-- #### DESCRIPTION OF PROCESS ####
-- Write a function to identify a prime number
-- Loop through all numbers from 2 to 10000 and save all the prime numbers
-- Reverse all prime numbers and save to a list 
-- Filter all the prime numbers and print to screen

-- First a function is declared that return true if an integer is a prime
-- Write source link here_______
isPrime :: Integral a => a -> Bool
isPrime k = length [x | x <- [2 .. k], k `mod` x == 0] == 1

-- All prime numbers are found between 2 and 10000 using the filter function
primes :: [Integer]
primes = filter isPrime [2 .. 10000]

-- A function to reverse an integer
reversal :: Integer -> Integer
reversal = read . reverse . show

-- Reverses all primes in a list
reversedPrimes :: [Integer]
reversedPrimes = map reversal primes

-- Returns all prime numbers in a list
reversibleStream :: [Integer]
reversibleStream = filter isPrime reversedPrimes

-- Test ideas :
--      - fetch a random element from the list and check if the reversal exists in the list
--      - Iterate over all the list and make sure each number is a prime
--      
-- 
-- I would test this function by ____
