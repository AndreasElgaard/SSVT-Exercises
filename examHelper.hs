module ExamHelper where
import           Data.List
infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Hoare Triples
hoareTest :: (a -> Bool) -> (a -> a) -> (a -> Bool) -> [a] -> Bool
hoareTest precondition f postcondition =
    all (\x -> precondition x --> postcondition (f x))

hoareTestR
    :: Fractional t
    => (a -> Bool)
    -> (a -> a)
    -> (a -> Bool)
    -> [a]
    -> (Bool, t)
hoareTestR precond f postcond testcases =
    let a = fromIntegral (length $ filter precond testcases)
        b = fromIntegral (length testcases)
    in  (all (\x -> precond x --> postcond (f x)) testcases, a / b)

invarTest :: (a -> Bool) -> (a -> a) -> [a] -> Bool
invarTest invar f = hoareTest invar f invar

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\x -> p x --> q x)
weaker xs p q = stronger xs q p

infixl 2 #
(#) :: (a -> b) -> (b -> c) -> (a -> c)
(#) = flip (.)

-- Negating a property
neg :: (a -> Bool) -> a -> Bool
neg p = \x -> not (p x)

-- Conjunctions and Disjunctions of Properties
infixl 2 .&&.
infixl 2 .||.
(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p .&&. q = \x -> p x && q x
(.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p .||. q = \x -> p x || q x

-- Set Helper Code
{-- Sets implemented as ordered lists without duplicates --}

newtype Set a = Set [a] deriving (Eq,Ord)

instance (Show a) => Show (Set a) where
    showsPrec _ (Set s) str = showSet s str

showSet []       str = showString "{}" str
showSet (x : xs) str = showChar '{' (shows x (showl xs str))
  where
    showl []       str = showChar '}' str
    showl (x : xs) str = showChar ',' (shows x (showl xs str))

emptySet :: Set a
emptySet = Set []

isEmpty :: Set a -> Bool
isEmpty (Set []) = True
isEmpty _        = False

inSet :: (Ord a) => a -> Set a -> Bool
inSet x (Set s) = elem x (takeWhile (<= x) s)

subSet :: (Ord a) => Set a -> Set a -> Bool
subSet (Set []      ) _   = True
subSet (Set (x : xs)) set = (inSet x set) && subSet (Set xs) set

inSetEq :: (Eq a) => a -> Set a -> Bool
inSetEq x (Set s) = x `elem` s

subSetEq :: (Eq a) => Set a -> Set a -> Bool
subSetEq (Set []      ) _   = True
subSetEq (Set (x : xs)) set = (inSetEq x set) && subSetEq (Set xs) set

insertSet :: (Ord a) => a -> Set a -> Set a
insertSet x (Set s) = Set (insertList x s)

insertList x []           = [x]
insertList x ys@(y : ys') = case compare x y of
    GT -> y : insertList x ys'
    EQ -> ys
    _  -> x : ys

deleteSet :: Ord a => a -> Set a -> Set a
deleteSet x (Set s) = Set (deleteList x s)

deleteList x []           = []
deleteList x ys@(y : ys') = case compare x y of
    GT -> y : deleteList x ys'
    EQ -> ys'
    _  -> ys

list2set :: Ord a => [a] -> Set a
list2set []       = Set []
list2set (x : xs) = insertSet x (list2set xs)

-- list2set xs = Set (foldr insertList [] xs)

powerSet :: Ord a => Set a -> Set (Set a)
powerSet (Set xs) = Set (sort (map (\xs -> (list2set xs)) (powerList xs)))

powerList :: [a] -> [[a]]
powerList []       = [[]]
powerList (x : xs) = (powerList xs) ++ (map (x :) (powerList xs))

takeSet :: Eq a => Int -> Set a -> Set a
takeSet n (Set xs) = Set (take n xs)

infixl 9 !!!

(!!!) :: Eq a => Set a -> Int -> a
(Set xs) !!! n = xs !! n

unionSet :: (Ord a) => Set a -> Set a -> Set a
unionSet (Set []      ) set2 = set2
unionSet (Set (x : xs)) set2 = insertSet x (unionSet (Set xs) set2)


-- Relations
type Rel a = [(a, a)]
isTransitive :: Eq a => Rel a -> Bool
isTransitive x = and [ (a, d) `elem` x | (a, b) <- x, (c, d) <- x, b == c ]

isReflexive :: Eq a => Rel a -> [a] -> Bool
isReflexive r = all (\x -> (x, x) `elem` r)

isIrreflexive :: Eq a => Rel a -> [a] -> Bool
isIrreflexive r ra = not (isReflexive r ra)

isAntisymmetric :: Eq a => Rel a -> Bool
isAntisymmetric r = and [ (y, x) `notElem` r || x == y | (x, y) <- r ]

isSymmetric :: Eq a => Rel a -> Bool
isSymmetric = not . isAntisymmetric
-- Assumption: A relation R is serial on a domain A if for ALL x ∈ A there is an y ∈ A such that xRy.
-- This differs from spec in Lab6 in the sense that serial rel implies for ALL x, not for any x.
-- As per: https://en.wikipedia.org/wiki/Serial_relation
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial a [] = True
-- Recursively, we are checking that for every relation in Rel a, both x (1st elem of tuple)
-- and y (2nd elem of tuple) exist within set a. If we have at least one (x, y) in Rel a such that
-- x, or y, are not elements of set a, we can confirm that the relation is NOT serial.
isSerial a (x : xs) =
    (inSetEq (fst x) (Set domainA) && inSetEq (snd x) (Set domainA))
        && isSerial a xs
    where domainA = nub a

