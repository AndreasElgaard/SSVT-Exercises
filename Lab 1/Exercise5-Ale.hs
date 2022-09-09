module Exercise5 where
import Test.QuickCheck

-- Checks if the input is a prime number
isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) (listOfPrimesN 2)

-- Creates a list of primes starting with the number N
-- Used to create a dynamic list of 101 consecutive prime numbers
listOfPrimesN :: Integer -> [Integer]
listOfPrimesN n = n : filter isPrime [n+1..]

-- Creates a list of 101 consectuve primes numbers 
-- startingPrime -> The first number of the prime list
take101Primes :: Integer -> [Integer]
take101Primes startingPrime = take 101 (listOfPrimesN startingPrime)

-- Steps for function
-- 1. Sums the array of primes
-- 2, Checks if the sum is prime
-- 3. If isPrime == True -> Output the sum (finished)
-- 4. Else -> rerun the function with a new list of 101 primes, starting with 
--            the next prime number after the current lowest prime in the list
checkSumPrimesIsPrime :: [Integer] -> (Integer, [Integer])
checkSumPrimesIsPrime [] = error "Unknown Error"
checkSumPrimesIsPrime (headOfPrimesArr: tailOfPrimesArr)
    | isSumPrime = (sumOfPrimes, headOfPrimesArr: tailOfPrimesArr)
    | otherwise = checkSumPrimesIsPrime (take101Primes (head tailOfPrimesArr))
    where isSumPrime = isPrime sumOfPrimes
          sumOfPrimes = sum (headOfPrimesArr: tailOfPrimesArr)

consecutive101Prime :: Integer
consecutive101Prime = sumOfPrimes
    where (sumOfPrimes, _) = checkSumPrimesIsPrime (take101Primes 2)

-- Q: Do you have to test that your answer is correct? How could this be checked?

-- A:  Yes, the function should be tested to asssure that:
--      a. The output is a prime number
--      b. The methodoldy used is valid:
--          - Does the array consist of only primes numbers
--          - Is the length of the array constantly 101 through iterations?
--          - Is the summation of the prime array the actual output

-- Time Spent:
--      2 hours -> First day was spent understanding the problem and getting acquisited with haskell
--                  Second day the solution was created via discussion


-- Tests
--  Didnt need to do tests, but wanted to verify code

-- Checks output is prime
proofIsOutputPrime :: Bool
proofIsOutputPrime = isPrime consecutive101Prime

-- Check length of Prime Arr == 101
proofLengthOfPrimeArr :: Bool
proofLengthOfPrimeArr = length(take101Primes 2) == 101

-- Checks if output is the sum of the prime array of primes
proofOfSumPrimes :: Bool
proofOfSumPrimes = sum arrOfPrimes == sumOfPrimes
    where (sumOfPrimes, arrOfPrimes) = checkSumPrimesIsPrime (take101Primes 2)

-- Test Report 
main :: IO Result
main = do
    putStrLn "\n=== Testing if output is Prime===\n"
    quickCheckResult proofIsOutputPrime
    putStrLn "\n=== Testing length of array == 101 ===\n"
    quickCheckResult proofLengthOfPrimeArr
    putStrLn "\n=== Testing the sum of the primes is valid ===\n"
    quickCheckResult proofOfSumPrimes