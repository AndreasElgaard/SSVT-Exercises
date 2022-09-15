module Exercise7 where
import Test.QuickCheck
import Data.Char

letterToDigits :: Char -> Int
letterToDigits char = case char of
 'A' -> 10
 'B' -> 11
 'C' -> 12
 'D' -> 13
 'E' -> 14
 'F' -> 15
 'G' -> 16
 'H' -> 17
 'I' -> 18
 'J' -> 19
 'K' -> 20
 'L' -> 21
 'M' -> 22
 'N' -> 23
 'O' -> 24
 'P' -> 25
 'Q' -> 26
 'R' -> 27
 'S' -> 28
 'T' -> 29
 'U' -> 30
 'V' -> 31
 'W' -> 32
 'X' -> 33
 'Y' -> 34
 'Z' -> 35
 code -> digitToInt code

stateIbanCharacters :: Num p => [Char] -> p
stateIbanCharacters state = case state of
    "AL" -> 28
    "AD" -> 24
    "AT" -> 20
    "AZ" -> 28
    "BH" -> 22
    "BY" -> 28
    "BE" -> 16
    "BA" -> 20
    "BR" -> 29
    "BG" -> 22
    "BI" -> 27
    "CR" -> 22
    "HR" -> 21
    "CY" -> 28
    "CZ" -> 24
    "DK" -> 18
    "DO" -> 28
    "EG" -> 29
    "SV" -> 28
    "EE" -> 20
    "FO" -> 18
    "FI" -> 18
    "FR" -> 27
    "GE" -> 22
    "DE" -> 22
    "GI" -> 23
    "GR" -> 27
    "GL" -> 18
    "GT" -> 28
    "VA" -> 22
    "HU" -> 28
    "IS" -> 26
    "IQ" -> 23
    "IE" -> 22
    "IL" -> 23
    "IT" -> 27
    "JO" -> 30
    "KZ" -> 20
    "XK" -> 20
    "KW" -> 30
    "LV" -> 21
    "LB" -> 28
    "LY" -> 25
    "LI" -> 21
    "LT" -> 20
    "LU" -> 20
    "MT" -> 31
    "MR" -> 27
    "MU" -> 30
    "MD" -> 24
    "MC" -> 27
    "ME" -> 22
    "NL" -> 18
    "MK" -> 19
    "NO" -> 15
    "PK" -> 24
    "PS" -> 29
    "PL" -> 28
    "PT" -> 25
    "QA" -> 29
    "RO" -> 24
    "LC" -> 32
    "SM" -> 27
    "ST" -> 25
    "SA" -> 24
    "RS" -> 22
    "SC" -> 31
    "SK" -> 24
    "SI" -> 19
    "ES" -> 24
    "SD" -> 18
    "SE" -> 24
    "CH" -> 21
    "TL" -> 23
    "TN" -> 24
    "TR" -> 26
    "UA" -> 29
    "AE" -> 23
    "GB" -> 22
    "VG" -> 24
    _ -> 0

mod97Algorithm :: (Integral a, Read a) => String -> a
mod97Algorithm ibanInInt = read ibanInInt `mod` 97

convertCharsToIntsIban :: [Char] -> [Char]
convertCharsToIntsIban iban = concatMap show (map letterToDigits iban)

moveInitCharsToEndIban :: [a] -> [a]
moveInitCharsToEndIban iban = amendedIban
    where
        amendedIban = shorterIban ++ firstFour
        shorterIban = drop 4 iban
        firstFour = take 4 iban

iban :: String -> Bool
iban val
    | stateIbanCharacters (take 2 val) /= length val = False
    | checkModulusIban val = True
    | otherwise = False

checkModulusIban val = modulus == 1 where 
        modulus = mod97Algorithm convertedToIntIban
        convertedToIntIban = convertCharsToIntsIban movedCharIban
        movedCharIban = moveInitCharsToEndIban val 