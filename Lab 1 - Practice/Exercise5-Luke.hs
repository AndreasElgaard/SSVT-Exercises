module Exercise5 where
import Data.List
import Test.QuickCheck    

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

consecutive101Prime :: Integer
consecutive101Prime = sum $ take 101 primes

consecutive100Prime :: Integer
consecutive100Prime = sum $ take 100 primes