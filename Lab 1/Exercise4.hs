
import Data.List
import Test.QuickCheck

--we are given this function
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

--this function is given in lab1.hs file we were given, so I got it from there. In the Lab questions,
-- its output was [Char] which gave me problems because prime only accepted Integer values, so I 
-- looked to change char to int with digitToInt but it didn't work because it's an array.
-- Then, I found this code which has 'read' on it, which reads what is inside the chars.
reversal :: Integer -> Integer
reversal = read . reverse . show

reversibleStream :: [Integer] 
reversibleStream = [ x | x <- [1..10000], prime x && prime(reversal x) ]


--If We Were to test it:

-- First test case: We will check all the numbers in our list are prime.
-- Second test case: We will check all the numbers in our list are reversal. (We assume one digit
--numbers are reversal by itself)

