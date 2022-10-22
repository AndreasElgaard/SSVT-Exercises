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

isAntisymmetric :: Eq a => Rel a -> Bool
isAntisymmetric r = and [ (y, x) `notElem` r || x == y | (x, y) <- r ]