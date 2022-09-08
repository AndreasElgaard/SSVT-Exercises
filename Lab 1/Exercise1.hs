
import Data.List
import Test.QuickCheck

leftPart:: Integer -> Integer
leftPart n = sum[k^2 | k <- [1..n]] 

rightPart:: Integer -> Integer
rightPart n = n*(n+1)*(2*n+1) `div` 6

exercise1:: Integer -> Bool
exercise1 n = leftPart n == rightPart n

testExercise1:: Integer -> 