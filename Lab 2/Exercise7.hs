module Exercise7 where
import Data.Char ( digitToInt )
-- TODO
--  Add some more comments 
--  Explain if this can be automatically tested
--  Time spent
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
    _ ->  0

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

checkModulusIban :: [Char] -> Bool
checkModulusIban val = modulus == 1 where 
        modulus = mod97Algorithm convertedToIntIban
        convertedToIntIban = convertCharsToIntsIban movedCharIban
        movedCharIban = moveInitCharsToEndIban val 

-- List of valid IBANs from https://www.iban.com/structure
listOfValidIbans :: [[Char]]
listOfValidIbans = [
    "AL35202111090000000001234567",
    "AD1400080001001234567890",
    "AT483200000012345864",
    "AZ77VTBA00000000001234567890",
    "BH02CITI00001077181611",
    "BY86AKBB10100000002966000000",
    "BE71096123456769",
    "BA393385804800211234",
    "BR1500000000000010932840814P2",
    "MT31MALT01100000000000000000123",
    "MU43BOMM0101123456789101000MUR",
    "PK36SCBL0000001123456702",
    "PS92PALS000000000400123456702",
    "PL10105000997603123456789123",
    "PT50002700000001234567833",
    "QA54QNBA000000000000693123456",
    "RO66BACX0000001234567890",
    "LC14BOSL123456789012345678901234",
    "SM76P0854009812123456789123",
    "ST23000200000289355710148",
    "SA4420000001234567891234",
    "RS35105008123123123173",
    "SC74MCBL01031234567890123456USD",
    "SK8975000000000012345671",
    "SI56192001234567892",
    "ES7921000813610123456789",
    "SD8811123456789012",
    "SE7280000810340009783242",
    "CH5604835012345678009",
    "TL380010012345678910106",
    "TN5904018104004942712345",
    "TR320010009999901234567890",
    "UA903052992990004149123456789",
    "AE460090000000123456789",
    "GB33BUKB20201555555555",
    "VG07ABVI0000000123456789"]

-- Checking that for every valid IBAN, function understands the iban to be valid
prop_checkValidIbans :: Bool
prop_checkValidIbans = all (==True) [checkModulusIban x | x <- listOfValidIbans]
-- Checking that for every invalid IBAN with additional character,
-- function understands the iban to be invalid
prop_checkInvalidIbansWithExtraCharacter :: Bool
prop_checkInvalidIbansWithExtraCharacter = all (==False) [checkModulusIban (x ++ "1") | x <- listOfValidIbans]
-- Checking that for every invalid IBAN without an initial country code,
-- function understands the iban to be invalid
prop_checkInvalidIbansWithoutCountryCode :: Bool
prop_checkInvalidIbansWithoutCountryCode = all (==False) [checkModulusIban (drop 2 x) | x <- listOfValidIbans]
-- Checking that for every invalid IBAN with an incorrect country code ("AA")
-- function understands the iban to be invalid
prop_checkInvalidIbansWithIncorrectCountryCode :: Bool
prop_checkInvalidIbansWithIncorrectCountryCode = all (==False) [checkModulusIban ("AA" ++ x) | x <- listOfValidIbans]

main :: IO ()
main = do 
      putStrLn "\n=== Testing a valid list of IBANs ===\n"
      print prop_checkValidIbans
      putStrLn "\n===  Testing invalid ibans by changing the values of valid IBANs ===\n"
      print prop_checkInvalidIbansWithExtraCharacter
      putStrLn "\n=== Testing IBANs with invalid country codes ===\n"
      print prop_checkInvalidIbansWithoutCountryCode
      putStrLn "\n=== Testing invalid IBANs with invalid country code ===\n"
      print prop_checkInvalidIbansWithIncorrectCountryCode
    
-- Automated testing: