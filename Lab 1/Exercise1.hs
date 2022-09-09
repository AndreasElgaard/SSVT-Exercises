module Exercise1 where
import Test.QuickCheck

squaredNumberSequenceLhs :: (Num a, Enum a) => a -> a
squaredNumberSequenceLhs n = foldr ((+) . (^2)) 0 [1..n]

cubedNumberSequenceLhs :: (Num a, Enum a) => a -> a
cubedNumberSequenceLhs n = foldr ((+) . (^3)) 0 [1..n]

squaredNumberSequenceRhs :: Integral a => a -> a
squaredNumberSequenceRhs n = n * (n + 1) * ((n*2) + 1) `div` 6

cubedNumberSequenceRhs :: Integral a => a -> a
cubedNumberSequenceRhs n = (n * (n + 1) `div` 2) ^ 2

-- Ref. to https://owlcation.com/stem/Why-ab-2-a2b22ab for derivation of formula from (a + b)^2
squaredNumberFormulaDefinition :: (Num b, Enum b) => b -> b -> b
squaredNumberFormulaDefinition n x = foldr ((+) . (\n -> (n-x)^2 + (x^2) + (2*(n-x)*x))) 0 [1..n]

-- Ref. to https://www.cuemath.com/a-plus-b-cube-formula/ for derivation of formula from (a + b)^3
cubedNumberFormulaDefinition :: (Num b, Enum b) => b -> b -> b
cubedNumberFormulaDefinition n x = 
    foldr ((+) . (\n -> (n-x)^3 + (x^3) + (3*(n-x)*x^2) + (3*((n-x)^2)*x))) 0 [1..n]

-- We want to proof that the series' of squared/cubed numbers in Exercise 2 and 3 respectively
-- is equivalent to the equations inducted that form the n^2/n^3 element sets.
-- Hence, to correctly test such inductive case, we want to verify that, for all natural
-- numbers, the Left Hand Side of the hypothesis (series of squared/cubed numbers up to n)
-- is equivalent to the Right Hand Side of the hypothesis (formula resulting from mathematical induction).

proof_squareFormula :: Integral a => a -> Property
proof_squareFormula n = n > 0 ==> squaredNumberSequenceLhs n == squaredNumberSequenceRhs n

proof_cubedFormula :: Integral a => a -> Property
proof_cubedFormula n = n > 0 ==> cubedNumberSequenceLhs n == cubedNumberSequenceRhs n

-- The additional proofs below further substatntiate that, by using the formula definitions
-- for (a + b)^2 and (a + b)^3, we can confirm that the sequence equations provided in 
-- Workshop 1 exercises 2 and 3 equate to the summation of squared/cubed values from 1 to n

proof_squareFormulaValidation :: Integral a => a -> a -> Property
proof_squareFormulaValidation n x = n > 0 && x > 0 ==> squaredNumberFormulaDefinition n x == squaredNumberSequenceRhs n

proof_cubedFormulaValidation :: Integral a => a -> a -> Property
proof_cubedFormulaValidation n x = n > 0 && x > 0 ==> squaredNumberFormulaDefinition n x== cubedNumberSequenceRhs n

