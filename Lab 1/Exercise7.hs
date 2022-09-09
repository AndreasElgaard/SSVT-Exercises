module Exercise7 where
import Test.QuickCheck

parity n = (length n - 2) `mod` 2

innerCheck dig par
    | dig `mod` 2 == par  = dig * 2
    | dig > 9 = dig - 9
    | otherwise = dig

sumOddPlaces::Integer -> Integer
sumOddPlaces n = 

sumEvenPlaces::Integer -> Integer
sumEvenPlaces n = h

finalSum::Integer -> Integer
finalSum n = sum sumOddPlaces n sumEvenPlaces n

luhn ::  Integer -> Bool 
luhn cardNumber
    |finalSum cardNumber `mod`10 == 0 = True
    | otherwise = False


-- luhn cardNumber = foldr ((+) . innerCheck . read) 0 (show cardNumber)


-- luhn cardNumber = foldr ((+) . innerCheck . read) 0 (show cardNumber)
