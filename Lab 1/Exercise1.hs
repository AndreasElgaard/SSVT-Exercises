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

-- squaredNumberFormulaDefinition n x = map ((n-x)^2 + (x^2) + (2*(n-x)*x)) [1..n]

proof_squareFormula :: Integral a => a -> Property
proof_squareFormula n = n > 0 ==> squaredNumberSequenceLhs n == squaredNumberSequenceRhs n

proof_cubedFormula :: Integral a => a -> Property
proof_cubedFormula n = n > 0 ==> cubedNumberSequenceLhs n == cubedNumberSequenceRhs n

-- We want to proof that the series' of squared/cubed numbers in Exercise 2 and 3 respectively
-- is equivalent to the equations inducted that form the n^2/n^3 element sets.
-- Hence, to correctly test such inductive case, we want to verify that, for all natural
-- numbers, the Left Hand Side of the hypothesis (series of squared/cubed numbers up to n)
-- is equivalent to the Right Hand Side of the hypothesis (formula resulting from mathematical induction).

