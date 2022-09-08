
import Data.List
import Test.QuickCheck



perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
insrt x [] = [[x]]
insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

factorial:: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- i thought to go from n and multiplying n times n-1 to it. n*n-1*n-2. to dlooked for a factorial code online http://progopedia.com/example/factorial/118/ it is a basic math function
-- so i just checked for the most used amongst other haskell users. 

exercise3:: Int -> Int
exercise3 n = factorial n

testExercise3:: Int -> Property
testExercise3 n = n > 0 ==> length(perms [1..n]) == exercise3 n

--sloow
