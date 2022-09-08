
import Data.List
import Test.QuickCheck

leftPart:: Integer -> Integer
leftPart n = sum[k^2 | k <- [1..n]] 

rightPart:: Integer -> Integer
rightPart n = n*(n+1)*(2*n+1) `div` 6

exercise1:: Integer -> Bool
exercise1 n = leftPart n == rightPart n

testExercise1:: Integer -> Property
testExercise1 n = n > 0 ==> leftPart n == rightPart n

--Now the cubic exercise.

leftPart3:: Integer -> Integer
leftPart3 n = sum[k^3 | k <- [1..n]] 

rightPart3:: Integer -> Integer
rightPart3 n = (n*(n+1) `div` 2)^2

exercise1cub:: Integer -> Bool
exercise1cub n = leftPart3 n == rightPart3 n

testExercise1cub:: Integer -> Property
testExercise1cub n = n > 0 ==> leftPart3 n == rightPart3 n