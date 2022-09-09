module Exercise6  where
-- Objective of the program: Refute that (p1×⋯×pn)+1 is a prime, where p's are prime numbers

-- Checks if Intger is Prime
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

-- Creates an array of primes
primes :: [Integer]
primes = 2 : filter prime [3..]

-- Calculates the product of an array + 1
primeProduct :: [Integer] -> Integer
primeProduct primesArr =  product primesArr + 1

-- Returns an array of N primes
takeNPrimes :: Int -> [Integer]
takeNPrimes n = take n primes

-- Checks if the lenght of the current array is equal to the desired length (constLengthOfFinalArr)
--    If the length is equal to constLengthOfFinalArr -> Return the final result
--    Else run the checkConsPrimes function to find more counter examples of the product of primes
checkLengthOfResult :: [([Integer], Integer)] -> Int -> Int -> [([Integer], Integer)]
checkLengthOfResult currentResult lengthOfPrimesArr constLengthOfFinalArr 
    | (length currentResult == constLengthOfFinalArr) = currentResult 
    | otherwise =  checkConsPrimes currentResult (lengthOfPrimesArr + 1) constLengthOfFinalArr

-- Variable Descriptions:
--    lengthOfPrimesArr => The length of array of primes
--    constLengthOfFinalArr => A constant that dictates how long the final array will be 
-- Function Step By Step
--    1. Create an array of N amount of prime numbers (based on lengthOfPrimesArr)
--    2. Calculate the product of the prime number array + 1
--    3. If the product is not a prime check the length of the current result array and add it to the list of results
--      - If the length of the result list is equal to constLengthOfFinalArr, output the result
--      - Otherwise run the function agian till the above statement is satisfied
--    4. If the product is a prime repeat the process with one more prime number added to the list
checkConsPrimes :: [([Integer], Integer)] -> Int -> Int -> [([Integer], Integer)]
checkConsPrimes currentResult lengthOfPrimesArr constLengthOfFinalArr
    |  not (prime productArr) =  checkLengthOfResult ((primesArr, productArr) : currentResult) lengthOfPrimesArr constLengthOfFinalArr 
    | otherwise = checkConsPrimes currentResult (lengthOfPrimesArr + 1) constLengthOfFinalArr
    where productArr =  primeProduct primesArr
          primesArr = takeNPrimes lengthOfPrimesArr

-- Assumptions made:
--  This function would run infinitly unless a final length of result array is specified, therefor constLengthOfFinalArr is introduced 
counterexamples ::  [([Integer], Integer)]
counterexamples = checkLengthOfResult [] lengthOfPrimesArr constLengthOfFinalArr
    where lengthOfPrimesArr = 1
          constLengthOfFinalArr = 4



-- Time Spent:
--      5 hours amongst all the participants of the group (1 hour working together counts as 1 hour)
