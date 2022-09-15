module Exercise7 where
import Test.QuickCheck
import Data.Char

convertCharsToIntsIban = map(\x -> ord x - 55) 
replaceLettersWithDigits :: [Char] -> Int
replaceLettersWithDigits n = listToInteger (map letterToDigits n)

moveInitCharsToEndIban iban = amendedIban
    where 
        amendedIban = shorterIban ++ firstFour 
        shorterIban = drop 4 iban
        firstFour = take 4 iban

letterToDigits :: Char -> Int
letterToDigits 'A' = 10
letterToDigits 'B' = 11
letterToDigits 'C' = 12
letterToDigits 'D' = 13
letterToDigits 'E' = 14
letterToDigits 'F' = 15
letterToDigits 'G' = 16
letterToDigits 'H' = 17
letterToDigits 'I' = 18
letterToDigits 'J' = 19
letterToDigits 'K' = 20
letterToDigits 'L' = 21
letterToDigits 'M' = 22
letterToDigits 'N' = 23
letterToDigits 'O' = 24
letterToDigits 'P' = 25
letterToDigits 'Q' = 26
letterToDigits 'R' = 27
letterToDigits 'S' = 28
letterToDigits 'T' = 29
letterToDigits 'U' = 30
letterToDigits 'V' = 31
letterToDigits 'W' = 32
letterToDigits 'X' = 33
letterToDigits 'Y' = 34
letterToDigits 'Z' = 35
letterToDigits code = digitToInt code

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

iban :: String -> Bool
iban val 
    | not (stateIbanCharacters (take 2 val) == length val) = False
    | otherwise = True
