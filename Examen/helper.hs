import           Data.List
import           Data.Tuple
import           LTS
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

-- ========================================================================
-- ====================================RELATIONS===========================
-- ========================================================================

type Rel a = [(a, a)]

isTransitive :: Eq a => Rel a -> Bool
isTransitive x = and [ (a, d) `elem` x | (a, b) <- x, (c, d) <- x, b == c ]

isAntiTransitive :: Eq a => Rel a -> Bool
isAntiTransitive x =
  not $ or [ (a, d) `elem` x | (a, b) <- x, (c, d) <- x, b == c ]

isPartialOrder :: Eq a => Rel a -> [a] -> Bool
isPartialOrder r d = isTransitive r && isReflexive r d && isAntisymmetric r

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

generateReflexiveRel :: Gen (Rel Int, [Int])
generateReflexiveRel = do
  lengthOfR <- chooseInt (1, 20)
  listOfT   <- vectorOf lengthOfR genEqTuple
  let unique = nub listOfT
  let domain = map fst unique
  return (listOfT, domain)

generateQReflexiveRel :: Gen (Rel Int, [Int])
generateQReflexiveRel = do
  (reflexiveR, domain) <- generateReflexiveRel
  newAddition          <- chooseInt (1, 10000)
  let newDomain = newAddition : domain
  return (reflexiveR, newDomain)

genEqTuple :: Gen (Int, Int)
genEqTuple = do
  i <- chooseInt (1, 20)
  return (i, i)

genEqTupleWI :: Rel Int -> Gen (Rel Int)
genEqTupleWI randomRel = do
  return (concatMap (\(x, y) -> [(x, x), (y, y)]) randomRel)

-- Symmetric closure functionality
-- Referred to  below for symmetric closure definition:
--      https://math24.net/closures-relations.html
--      The Haskell Road to Logic, Math and Programming: Ch 5
symClos :: Ord a => Rel a -> Rel a
symClos []       = []
-- we use nub function to eliminate duplicate pairs
-- (inverse of (x, x) is (x,x), hence the inverse is identical)
symClos [x     ] = nub (createSymmetricTuples x)
symClos (x : xs) = nub (createSymmetricTuples x ++ symClos xs)

createSymmetricTuples :: (a, a) -> [(a, a)]
createSymmetricTuples a = a : [swap a]

genRTuple :: Gen (Int, Int)
genRTuple = do
  x <- chooseInt (1, 100)
  y <- chooseInt (1, 100)
  return (x, y)
-- ]verboseCheck $ forAll generateReflexiveRel $ prop_checkReflRDomain
prop_checkReflRDomain :: (Rel Int, [Int]) -> Bool
prop_checkReflRDomain (r, d) = isReflexive r d && isQuasiReflexive r

-- verboseCheck $ forAll generateQReflexiveRel  $ prop_checkIsReflex ==> This will fail because reflixve is not a subset of Qreflexive
-- verboseCheck $ forAll generateQReflexiveRel  $ prop_checkIsQReflex  ==> This works becuase Qreflex is a subset of reflexive

prop_checkIsQReflex :: Eq a => (Rel a, b) -> Bool
prop_checkIsQReflex (r, d) = isQuasiReflexive r
prop_checkIsReflex :: Eq a => (Rel a, [a]) -> Bool
prop_checkIsReflex (r, d) = isReflexive r d

hasUnreachableStates :: [(Int, String, Int)] -> Bool
hasUnreachableStates t = not $ all (\(i, t, o) -> o `elem` inputs) t
  where inputs = map (\(x, y, z) -> x) t

-- ========================================================================
-- ==========================LTS===========================================
-- ========================================================================

after :: IOLTS -> [Label] -> [State]
after (states, labelsI, labelsO, transitions, init) sigma | null sigma = [init]
                                                          | null final = []
                                                          | otherwise  = final
 where
  final               = tausFinalState ++ last afterImplementation
  tausFinalState      = findTaus transitions (last afterImplementation)
  afterImplementation = findAfter [init] sigma transitions


findAfter :: [State] -> [Label] -> [LabeledTransition] -> [[State]]
findAfter _ [] _ = []

findAfter s (l1 : otherLabels) trans
  | null findingFirstTransitionsSet
  = []
  | not (null findingFirstSet)
  = findingFirstSet : findAfter newState otherLabels trans
  | otherwise
  = []
 where
  findingFirstTransitionsSet = findTransitionsWithInitState (head s) trans
  findingFirstSet = findFinalStateFromLabel l1 findingFirstTransitionsSet
  newState = findingFirstSet

findTaus :: [LabeledTransition] -> [State] -> [State]
findTaus [] _ = []
findTaus ((s1, label, s2) : transitions) stat
  | label == "tau" && s1 `elem` stat = s2 : findTaus transitions stat
  | otherwise                        = findTaus transitions stat

findTransitionsWithInitState
  :: State -> [LabeledTransition] -> [LabeledTransition]
findTransitionsWithInitState _ [] = []
findTransitionsWithInitState init ((s1, label, s2) : transitions)
  | init == s1 = (s1, label, s2) : findTransitionsWithInitState init transitions
  | init /= s1 && null transitions = []
  | otherwise = findTransitionsWithInitState init transitions

findFinalStateFromLabel :: Label -> [LabeledTransition] -> [State]
findFinalStateFromLabel _ [] = []
findFinalStateFromLabel l1 ((s1, label, s2) : transitions)
  | l1 == label = s2 : findFinalStateFromLabel l1 transitions
  | otherwise   = []

out :: IOLTS -> [Label] -> [Label]
out (states, labelsI, labelsO, transitions, init) sigma = noRepetitions
 where
  noRepetitions    = removeDuplicateLabelsTau findingAllLabels sigma
  findingAllLabels = findLabelsForState stateSet transitions
  -- using the states from the after function defined in ex 4, and getting the labels
  -- achievable from transitions originating from such states
  stateSet         = after iolts sigma
  iolts            = (states, labelsI, labelsO, transitions, init)


findLabelsForState :: [State] -> [LabeledTransition] -> [Label]
findLabelsForState _ [] = []
findLabelsForState listStates ((s1, label, s2) : transitions)
  | s1 `elem` listStates = label : findLabelsForState listStates transitions
  | otherwise            = findLabelsForState listStates transitions

-- Removes the labels that were in the sigma list (list of labels we want to check) and also
-- all the tau labels.
-- This is according to Chapter 4.1 of Tretman's paper.

removeDuplicateLabelsTau :: [Label] -> [Label] -> [Label]
removeDuplicateLabelsTau [] _ = []
removeDuplicateLabelsTau (l1 : other) sigma
  | l1 `elem` sigma || l1 == "tau" = removeDuplicateLabelsTau other sigma
  | otherwise                      = l1 : removeDuplicateLabelsTau other sigma
