module Exercise6  where
import Data.List
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

primeProduct :: [Integer] -> Integer
primeProduct primesArr =  product primesArr + 1

takeNPrimes :: Int -> [Integer]
takeNPrimes n = take n primes

checkLengthOfResult :: [([Integer], Integer)] -> Int -> Int -> [([Integer], Integer)] 
checkLengthOfResult currentResult finalLength currentCounter 
    | length currentResult == finalLength = currentResult
    | otherwise = checkConsPrimes (currentCounter + 1) finalLength

checkConsPrimes :: Int -> Int -> [([Integer], Integer)]
checkConsPrimes currentCounter finalLength
    |  not (prime productArr) = checkLengthOfResult [(primesArr, productArr)] finalLength currentCounter
    | otherwise = checkConsPrimes(currentCounter + 1) finalLength
    where productArr =  primeProduct primesArr
          primesArr = takeNPrimes currentCounter

-- Works when lengthOfArr == 1, after that it seems to hang, not sure if its a bug or just the math
-- Maybe im missing some close condition
counterexamples ::  [([Integer], Integer)]
counterexamples = checkConsPrimes lengthOfArr finalLengthOfArr
    where lengthOfArr = 3
          finalLengthOfArr = 2