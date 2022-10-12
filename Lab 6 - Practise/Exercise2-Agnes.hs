import SetOrd

setUnion ::  Ord a => Set a -> Set a -> Set a
setUnion = unionSet 


setIntersection ::   Ord a => Set a -> Set a -> Set a
setIntersection (Set []) set2 = emptySet
setIntersection set1 (Set []) = emptySet
setIntersection (Set (x:xs)) (Set (y:ys)) 
    | x == y = insertSet x (setIntersection (Set xs) (Set ys))
    | x > y = setIntersection (Set (x:xs)) (Set ys)
    | otherwise = setIntersection (Set xs) (Set (y:ys))

-- this does not work
setDifference ::  Ord a => Set a -> Set a -> Set a
setDifference (Set []) set2 = emptySet
setDifference set1 (Set []) = emptySet
setDifference (Set (x:xs)) (Set (y:ys)) 
    | x == y = setDifference (Set xs) (Set ys)
    | x < y = insertSet x (setDifference (Set (x:xs)) (Set ys))
    | otherwise = setDifference (Set xs) (Set (y:ys))
