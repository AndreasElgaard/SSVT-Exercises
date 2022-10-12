import SetOrd

setUnion ::  Ord a => Set a -> Set a -> Set a
setUnion (Set [])     set2  =  set2
setUnion (Set (x:xs)) set2  = 
   insertSet x (setUnion (Set xs) set2)


setIntersection ::   Ord a => Set a -> Set a -> Set a
setIntersection (Set []) set2 = emptySet
setIntersection set1 (Set []) = emptySet
setIntersection (Set (x:xs)) (Set (y:ys)) 
    | x == y = insertSet x (setIntersection (Set xs) (Set ys))
    | x > y = setIntersection (Set (x:xs)) (Set ys)
    | otherwise = setIntersection (Set xs) (Set (y:ys))
