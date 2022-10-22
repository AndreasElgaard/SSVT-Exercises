import           Debug.Trace
import           Test.QuickCheck
numK :: Int -> Int -> Int
numK n 0 = trace ("!!KILL LOOP = n = " ++ show n ++ " k == " ++ show 0) $ 0
numK n 1 = trace ("!!KILL LOOP = n = " ++ show n ++ " k == " ++ show 1) $ 1
numK n k = sum $ map
    (\x ->
        trace ("n = " ++ show n ++ " k == " ++ show k ++ " X == " ++ show x)
            $ numK (n - x) (k - 1)
    )
    [0 .. n]

genPos :: Gen Int
genPos = abs `fmap` (arbitrary :: Gen Int) `suchThat` (\x -> x >= 0 && x < 20)

isTrue :: Int -> Int -> Bool
isTrue n k = (numK n k) >= 0

type Rel a = [(a, a)]

prop_test :: Rel Int -> Bool
prop_test inp = length inp >= 0

isTransitive :: Rel Int -> Bool
isTransitive x = and [ (a, d) `elem` x | (a, b) <- x, (c, d) <- x, b == c ]

isReflexive :: Rel Int -> [Int] -> Bool
isReflexive r = all (\x -> (x, x) `elem` r)

isIrreflexive :: Rel Int -> [Int] -> Bool
isIrreflexive r ra = not (isReflexive r ra)

isAntisymmetric :: Rel Int -> Bool
isAntisymmetric r = and [ (y, x) `notElem` r || x == y | (x, y) <- r ]

isSymmetric :: Rel Int -> Bool
isSymmetric = not . isAntisymmetric


-- numKUnique :: Int -> Int -> Int
-- numKUnique n k = map () [0..n]

corona1 :: Num a => a -> a -> a -> Int -> a
corona1 r s x0 t = iterate ((s +) . (r *)) x0 !! t
-- corona2 :: (Fractional a, Integral b) => a -> a -> a -> b -> a
corona2 r s x0 t = (r ^ t - 1) / (r - 1) * s + r ^ t * x0
corona3 :: (Num a, Integral b) => a -> a -> a -> b -> a
corona3 r s x0 t = foldr (-) x0 [ s + r ^ d * x0 | d <- [0 .. t] ]
corona4 :: Num a => a -> a -> a -> Int -> a
corona4 r s x0 t = (!! t) $ zipWith (+) (iterate (+ s) 0) (iterate (* r) x0)
corona5 :: Num c => c -> c -> c -> Int -> c
corona5 r s x0 t = head . drop t $ iterate (\c -> s + r * c) x0

corona6 :: (Integral b, Integral a) => a -> a -> a -> b -> a
corona6 r s x0 t = (r ^ t) * x0 + s * ((1 - (r ^ t)) `div` (1 - r))
