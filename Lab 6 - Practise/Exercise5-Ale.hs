module Exercise5 where

import Exercise3 ( Rel )
import Data.List(nub)

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos ::  Ord a => Rel a -> Rel a
trClos rel = rel


mapThrough [] = []
mapThrough ((a,b):xs) = map (\(x,y) -> checkTuple (a,b) (x,y)) xs

checkTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
checkTuple (a,b) (c,d)
  | b == c = (a,d)
  | otherwise = (undefined, undefined)
