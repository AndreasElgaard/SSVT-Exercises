module Exercice1 where
import           Data.List
import           MultiplicationTable
import           Mutation
import           Test.QuickCheck

test = generate $ sequence $ replicate
    20
    (mutate removeElements prop_tenElements multiplicationTable 1)


multiplyElements :: [Integer] -> Gen [Integer]
multiplyElements xs = do
    num <- arbitrary :: Gen Integer
    return (map (* num) xs)

changeOrder :: [Integer] -> Gen [Integer]
changeOrder xs = do
    return (reverse xs)

multiplyByAListOfInts :: [Integer] -> Gen [Integer]
multiplyByAListOfInts xs = do
    let list = zip [1 .. 10] xs
    return (map (uncurry (*)) list)

-- Adds each integer in the list by 1 to invalidate the modulus check
addForModulus :: [Integer] -> Gen [Integer]
addForModulus xs = do
    return (map (+ 1) xs)

-- Replaces a random integer in the list with a new random value
changeRandomElement :: [Integer] -> Gen [Integer]
changeRandomElement xs = do
    numLength <- choose (1, length xs)
    num       <- arbitrary :: Gen Integer
    let (first, second) = splitAt numLength xs
    let tailOf          = customTail second
    return (first ++ [num] ++ tailOf)

-- Generates a completely random list of integers
-- This mutator is very weak because it caters for a very large set of
totallyRandom :: [Integer] -> Gen [Integer]
totallyRandom xs = do
    arbitrary :: Gen [Integer]

customTail :: [a] -> [a]
customTail []       = []
customTail (x : xs) = xs

