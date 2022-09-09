
import Data.List
import Test.QuickCheck

--we are given this function
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

consecutive101Prime :: Integer
consecutive101Prime = all(\ x -> prime x)
  where end = takeWhile (length(consecutive101Prime) < 101)




