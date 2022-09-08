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

proof_squareFormula :: Integral a => a -> Property
proof_squareFormula n = n > 0 ==> squaredNumberSequenceLhs n == squaredNumberSequenceRhs n

proof_cubedFormula :: Integral a => a -> Property
proof_cubedFormula n = n > 0 ==> cubedNumberSequenceLhs n == cubedNumberSequenceRhs n

