module Exercise7 where
import Test.QuickCheck

parity n = (length n - 2) `mod` 2

innerCheck dig par
    | dig `mod` 2 == par  = dig * 2
    | dig > 9 = dig - 9
    | otherwise = dig


-- luhn cardNumber = foldr ((+) . innerCheck . read) 0 (show cardNumber)
