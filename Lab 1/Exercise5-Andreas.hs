module Exercise5 where

--Write a function to identify a prime number
-- First i want to fill a list with 101 consecutive prime numbers
-- First i want to create a list of 101 consecutive prime numbers from the lowest prime number at index 0 and the highest at index 100. 
-- If the sum of the list is not equal to a prime number, index 0 is popped and the next prime is found which will recieve index 100.
-- Recursion will be used to contine this until the sum of the list is equal to a prime number.

-- First a function is declared that return true if an integer is a prime
isPrime :: Integral a => a -> Bool
isPrime k = length [x | x <- [2 .. k], k `mod` x == 0] == 1

-- List of 101 consecutive prime numbers
listPrimeNumbers :: [Int]
listPrimeNumbers = []

-- Finds the next prime number
nextPrime :: Int -> Int
nextPrime n | isPrime n = n
            | otherwise = nextPrime (n+1)
