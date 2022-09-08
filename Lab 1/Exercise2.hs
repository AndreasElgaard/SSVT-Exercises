
import Data.List
import Test.QuickCheck



-- I dont need all the list, bc enunciat. exercise2:: [Integer] -> Bool
--exercise2 x = length (subsequences x) == 2^x

ex2 :: Integer -> Bool 
ex2 x = 2 ^ x == length (subsequences [1..x])

testEx2:: Integer -> Property
testEx2 x = x > 0 ==>  2 ^ x == length (subsequences [1..x])

--it's very slow because of the 2^powerup.