
import Data.List
import Test.QuickCheck

--we are given this function
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

--primeList :: [Integer]
--primeList = [ x | x <- [1..n], prime x ]

sumPrime :: Integer -> Integer
sumPrime n = sum [take n primes + 1]

funct :: Integer -> [Integer] -> [([Integer], Integer)]
funct summed (x:xs) 
  | prime summed  = [(x, summed) ]
  | otherwise = funct sumPrime xs

counterexamples::  [([Integer], Integer)]
counterexamples = counterExamples' 5

counterExamples' :: Integer -> [([Integer], Integer)]
counterExamples' n = (some_primes, sum_some_primes) : counterExamples' n + 1
  where
    some_primes = 
              


--counterexamples [([2,5,7],15)]



