import           Data.List
import           Test.QuickCheck
infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

hoareTest :: (a -> Bool) -> (a -> a) -> (a -> Bool) -> [a] -> Bool
hoareTest precondition f postcondition =
  all (\x -> precondition x --> postcondition (f x))


stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\x -> p x --> q x)
weaker xs p q = stronger xs q p

type Rel a = [(a, a)]

isTransitive :: Eq a => Rel a -> Bool
isTransitive x = and [ (a, d) `elem` x | (a, b) <- x, (c, d) <- x, b == c ]

isReflexive :: Eq a => Rel a -> [a] -> Bool
isReflexive r = all (\x -> (x, x) `elem` r)

isQuasiReflexive :: Eq a => Rel a -> Bool
isQuasiReflexive r = and [ (x, x) `elem` r && (y, y) `elem` r | (x, y) <- r ]

isIrreflexive :: Eq a => Rel a -> [a] -> Bool
isIrreflexive r = not . any (\x -> (x, x) `elem` r)

isAntisymmetric :: Eq a => Rel a -> Bool
isAntisymmetric r = and [ (y, x) `notElem` r || x == y | (x, y) <- r ]

isSymmetric :: Eq a => Rel a -> Bool
isSymmetric r = and [ (y, x) `elem` r | (x, y) <- r ]

isAsymmetric :: Eq a => Rel a -> [a] -> Bool
isAsymmetric r d = isIrreflexive r d && isAntisymmetric r

isEquivalent :: Eq a => Rel a -> [a] -> Bool
isEquivalent r d = isReflexive r d && isSymmetric r && isTransitive r

isLinear :: Eq a => Rel a -> Bool
isLinear r = or [ (x, y) `elem` r || (y, x) `elem` r || x == y | (x, y) <- r ]

compose :: Eq a => Rel a -> Rel a -> Rel a
compose r1 r2 =
  concatMap (\(x, y) -> concatMap (\(x2, y2) -> [ (x, y2) | y == x2 ]) r2) r1

squareCompose :: Eq a => Rel a -> Rel a
squareCompose r =
  concatMap (\(x, y) -> concatMap (\(x2, y2) -> [ (x, y2) | y == x2 ]) r) r

inverseRel :: Rel a -> Rel a
inverseRel = map (\(x, y) -> (y, x))

inverseCompose :: Eq a => Rel a -> Rel a
inverseCompose r = compose r inv where inv = inverseRel r


generateRel :: Gen (Rel Int)
generateRel =
  (arbitrary :: Gen (Rel Int))
    `suchThat` (\x -> length x > 2 && isReflexive x [0 .. 10])
    -- >>=        \x -> return $ sort (nub x)

prop_isQuasi :: Rel Int -> Bool
prop_isQuasi = isQuasiReflexive
