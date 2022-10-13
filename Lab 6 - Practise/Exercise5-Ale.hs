module Exercise5 where

import Exercise3 ( Rel )
import Data.List(nub, elemIndex)

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]


trClos :: Eq a => Rel a -> Rel a
trClos = iterateWIndex 0

-- findAndReplaceInList inTuple fullList =

iterateWIndex :: Eq a => Int -> Rel a -> Rel a
iterateWIndex counter [] = []
iterateWIndex counter xy@(x:xs)
  | (counter + 1) == length xy = checkTuple' xy
  | otherwise =  checkTuple' xy ++ iterateWIndex (counter+1) (move (counter+1) xy)


checkTuple' :: Eq a => Rel a -> Rel a
checkTuple' [] = []
checkTuple' ((x1,x2):(y1,y2):xs)
  | x2 == y1 = (x1,y2) : checkTuple' ((x1,x2):xs)
  | otherwise = checkTuple' ((x1,x2):xs)
checkTuple' ((x1,x2):xs)
  | null xs = []
  | x2 == y1 = (x1,y2) : checkTuple' ((x1,x2):[])
  | otherwise = checkTuple' ((x1,x2):[])
      where (y1,y2) = head xs

-- Taken from https://stackoverflow.com/questions/1041440/how-to-move-an-element-in-a-list-in-haskell
move :: Int -> [a] -> [a]
move n as = head ts : (hs ++ tail ts)
   where (hs, ts) = splitAt n as
