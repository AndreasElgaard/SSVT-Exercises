module Exercise2 where
import           Exercise1
import           Lecture3
import           Test.QuickCheck

-- Time spend: x minutes --

-- There is a few ways to test the parse-function (see Lecture3.hs). The first approch would be to create manual tests
-- where the input and the output would be compared and return True if equal.
-- It could also be possible to do automated testing by parsing automatically generated forms.
-- The group chose to create manual tests.

-- =================================== Manual Test =================================
-- Function that compares the output of the parse-function with the output of a Form.
manualTestParse :: [Form] -> String -> Bool
manualTestParse form str = form == parse str

-- =================================== Props =================================
-- The following props tests whether or not a parse returns an embty formula if it is invalid.
-- The props will also check if a formula string is equvilant to a form object.
-- To test the proberly will all operaters in the Form-data type be tested.

-- Testing if parse and form are equal to each other. Form1 is being tested.
prop_isCorrectParse1 :: Bool
prop_isCorrectParse1 = manualTestParse [form1] "((1==>2)<=>(-2==>-1))"

-- Testing if parse and form are equal to each other. Form2 is being tested.
prop_isCorrectParse2 :: Bool
prop_isCorrectParse2 = manualTestParse [form2] "((1==>2)<=>(-1==>-2))"

-- Testing if parse and form are equal to each other. Form3 is being tested.
prop_isCorrectParse3 :: Bool
prop_isCorrectParse3 = manualTestParse [form3] "(*((1==>2) (2==>3))==>(1==>3))"

-- Testing if parse returns invalid formula
prop_isInvalidFormula :: Bool
prop_isInvalidFormula = null (parse "(-1 -2)")

--PROP
-- Testing if parse and form are equal to each other. Here the Prop-Form is being tested
prop_isParsePropCorrect :: Bool
prop_isParsePropCorrect = manualTestParse [Prop 2] "2"

-- Testing if parse and form are NOT equal to each other. Here the Prop-Form is being tested
prop_isParsePropNotCorrect :: Bool
prop_isParsePropNotCorrect = not (manualTestParse [Prop 1] "-2")

-- NEG
-- Testing if parse and form are equal to each other. Here the Neg-Form is being tested
prop_isParseNegCorrect :: Bool
prop_isParseNegCorrect = manualTestParse [Neg (Prop 2)] "-2"

-- Testing if parse and form are NOT equal to each other. Here the Neg-Form is being tested
prop_isParseNegNotCorrect :: Bool
prop_isParseNegNotCorrect = not (manualTestParse [Neg (Prop 1)] "1")

-- CNJ
-- Testing if parse and form are equal to each other. Here the Cnj-Form is being tested
prop_isParseCnjCorrect :: Bool
prop_isParseCnjCorrect =
  manualTestParse [Cnj [Neg (Prop 1), Neg (Prop 2)]] "*(-1 -2)"

-- Testing if parse and form are NOT equal to each other. Here the Cnj-Form is being tested
prop_isParseCnjNotCorrect :: Bool
prop_isParseCnjNotCorrect =
  not (manualTestParse [Cnj [Neg (Prop 1), Neg (Prop 2)]] "*(1 -2)")

-- DSJ
-- Testing if parse and form are equal to each other. Here the Dsj-Form is being tested
prop_isParseDsjCorrect :: Bool
prop_isParseDsjCorrect =
  manualTestParse [Dsj [Neg (Prop 1), Neg (Prop 2)]] "+(-1 -2)"

-- Testing if parse and form are NOT equal to each other. Here the Dsj-Form is being tested
prop_isParseDsjNotCorrect :: Bool
prop_isParseDsjNotCorrect =
  not (manualTestParse [Dsj [Neg (Prop 1), Neg (Prop 2)]] "+(1 -2)")

-- IMPL
-- Testing if parse and form are equal to each other. Here the Impl-Form is being tested
prop_isParseImplCorrect :: Bool
prop_isParseImplCorrect = manualTestParse
  [Impl (Dsj [Neg (Prop 1), Neg (Prop 2)]) (Dsj [Neg (Prop 2), Neg (Prop 1)])]
  "(+(-1 -2)==>+(-2 -1))"

-- Testing if parse and form are NOT equal to each other. Here the Impl-Form is being tested
prop_isParseImplNotCorrect :: Bool
prop_isParseImplNotCorrect = not
  (manualTestParse
    [Impl (Dsj [Neg (Prop 1), Neg (Prop 2)]) (Dsj [Neg (Prop 2), Neg (Prop 1)])]
    "(*(-1 -2)==>+(-2 -1))"
  )

-- Equiv
-- Testing if parse and form are equal to each other. Here the Equiv-Form is being tested
prop_isParseEquivCorrect :: Bool
prop_isParseEquivCorrect = manualTestParse [Equiv (Prop 2) (Prop 1)] "(2<=>1)"

-- Testing if parse and form are NOT equal to each other. Here the Equiv-Form is being tested
prop_isParseEquivNotCorrect :: Bool
prop_isParseEquivNotCorrect =
  not (manualTestParse [Equiv (Prop 1) (Prop 2)] "(2<=>1)")

-- =================================== Test Report ===================================
main :: IO Result
main = do
  putStrLn
    "\n=== Testing if parse and form are equal to each other. Form1 is being tested. ===\n"
  quickCheckResult prop_isCorrectParse1

  putStrLn
    "\n=== Testing if parse and form are equal to each other. Form2 is being tested. ===\n"
  quickCheckResult prop_isCorrectParse2

  putStrLn
    "\n=== Testing if parse and form are equal to each other. Form3 is being tested. ===\n"
  quickCheckResult prop_isCorrectParse3

  putStrLn "\n=== Testing if parse returns invalid formula. ===\n"
  quickCheckResult prop_isInvalidFormula

  putStrLn
    "\n=== Testing if parse and form are equal to each other. Here the Prop-Form is being tested ===\n"
  quickCheckResult prop_isParsePropCorrect

  putStrLn
    "\n=== Testing if parse and form are NOT equal to each other. Here the Prop-Form is being tested ===\n"
  quickCheckResult prop_isParsePropNotCorrect

  putStrLn
    "\n=== Testing if parse and form are equal to each other. Here the Neg-Form is being tested ===\n"
  quickCheckResult prop_isParseNegCorrect

  putStrLn
    "\n=== Testing if parse and form are NOT equal to each other. Here the Neg-Form is being tested ===\n"
  quickCheckResult prop_isParseNegNotCorrect

  putStrLn
    "\n=== Testing if parse and form are equal to each other. Here the Cnj-Form is being tested ===\n"
  quickCheckResult prop_isParseCnjCorrect

  putStrLn
    "\n=== Testing if parse and form are NOT equal to each other. Here the Cnj-Form is being tested ===\n"
  quickCheckResult prop_isParseCnjNotCorrect

  putStrLn
    "\n=== Testing if parse and form are equal to each other. Here the Dsj-Form is being tested ===\n"
  quickCheckResult prop_isParseDsjCorrect

  putStrLn
    "\n=== Testing if parse and form are NOT equal to each other. Here the Dsj-Form is being tested ===\n"
  quickCheckResult prop_isParseDsjNotCorrect

  putStrLn
    "\n=== Testing if parse and form are equal to each other. Here the Impl-Form is being tested ===\n"
  quickCheckResult prop_isParseImplCorrect

  putStrLn
    "\n=== Testing if parse and form are NOT equal to each other. Here the Impl-Form is being tested ===\n"
  quickCheckResult prop_isParseImplNotCorrect

  putStrLn
    "\n=== Testing if parse and form are equal to each other. Here the Equiv-Form is being tested ===\n"
  quickCheckResult prop_isParseEquivCorrect

  putStrLn
    "\n=== Testing if parse and form are NOT equal to each other. Here the Equiv-Form is being tested ===\n"
  quickCheckResult prop_isParseEquivNotCorrect

