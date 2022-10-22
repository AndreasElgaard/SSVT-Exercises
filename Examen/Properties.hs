import           Data.List
import           Data.Tuple
import           Test.QuickCheck

type Rel a = [(a, a)]

-- time spent: 30m



--https://www.ics.uci.edu/~alspaugh/cls/shr/relation.html

-- Properties:

isTransitive :: Eq a => Rel a -> Bool
isTransitive x = and [ (a,d) `elem` x | (a,b) <-x, (c,d) <-x, b==c]

isReflexive :: Eq a => Rel a ->[a]-> Bool
isReflexive r = all (\x -> (x,x) `elem` r)

isAntisymmetric::Eq a => Rel a -> Bool
isAntisymmetric r = and [(y,x) `notElem` r || x==y |(x,y) <- r]

--Code for:

-- =========================== INVERSE =================================================
    
inverse:: Eq a => Rel a -> Rel a
inverse = map swap 

-- =========================== COMPOSITION =================================================
--CHANGE   
-- composition relation {(x,z)∈X×Z | xRy and ySz for some y∈Y}. 

composeR :: Eq a => Rel a -> Rel a -> Int -> Rel a
composeR r1 r2 counter
  | counter == 1000 = r1
  | counter == 0 = composeR r1 newr2 (counter+1)
  | r1 == r2 = r1
  | otherwise = composeR r1 newr2 (counter+1)
  where newr2 = concatMap (\(x,y) -> concatMap (\(x2, y2) -> [(x,y2) | y == x2]) r1) r2


-- Product -> two relations R and S is the relation {(w,x,y,z) | wRx∧yRz} }

--Transpose   of R, written R−1, is the relation {(y,x) | xRy}. 

-- Closure of a relation R is the relation {(x,z) | (x,y)∈R∧(y,z)∈R}. 

-- Transitive closure of R is the smallest transitive relation S such that R⊆S.  

-- Intersection    of R and S, written RS, is the relation {x(RS)y | xRy and xSy}.

-- =========================== UNION=================================================

-- The union of R and S, written R∪S, is the relation {x(R∪S)y | xRy or xSy}.

unionRel :: (Ord a) => Rel a -> Rel a -> Rel a
unionRel [] rel2 = sort rel2
unionRel (x : xs) rel2 = sort $ insertRel x (unionRel xs rel2)

insertRel :: (Ord a) => (a, a) -> [(a, a)] -> [(a, a)]
insertRel = insertList

insertList x y
  | x `elem` y = y
  | otherwise = x : y


-- Union of R with R^{-1} -- def. of Symmetric Closure
symCloseUnion :: Ord a => Rel a -> Rel a
symCloseUnion r = unionRel r inverseOfR where inverseOfR = map swap r

-- =========================== DIFFERENCE =================================================
    


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

-- for each tuple within binary relation, we add to our result list the tuple itself,
-- and its inverse (using swap function)
createSymmetricTuples :: (a, a) -> [(a, a)]
createSymmetricTuples a = a : [swap a]