module Exercise5 where

import Exercise3
import Data.List(nub, elemIndex)
import Test.QuickCheck

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- trClos :: Eq a => Rel a -> Rel a
-- trClos rels = nub (iterateWIndex 0 rels)


-- fix :: (a -> a) -> a
-- fix f = f (fix f)
-- infixl 1 $$

-- ($$) :: a -> (a -> b) -> b
-- ($$) = flip ($)
-- test :: Rel a -> Rel a
-- test rels = (0, rels, []) $$
--   fix (\ f (counter, relList,outPut) -> if (counter+1) == length then outPut else f (counter+1, tail relList, (relList !! counter) @@ relList))

trClos :: Eq a => Rel a -> Rel a
trClos rels = nub (iterateWIndex 0 rels)

-- Iterates over the provided list of relations
--    If the counter+1 is equal to the length of the list, the final run is made
--    Otherwise, Run check tuple for the current full list, the head of the list is assumed to be
--    the tuple which is currently being processed
iterateWIndex :: Eq a => Int -> Rel a -> Rel a
iterateWIndex counter [] = []
iterateWIndex counter xy@(x:xs)
  | (counter + 1) == length xy = checkTuple' xy
  | otherwise =  checkTuple' xy ++ iterateWIndex (counter+1) (move (counter+1) xy)

--
checkTuple' :: Eq a => Rel a -> Rel a
checkTuple' [] = []
checkTuple' ((x1,x2):(y1,y2):xs)
  | x2 == y1 = (x1,x2): (x1,y2) : checkTuple' ((x1,x2):xs) ++ checkTuple' ((x1,y2):xs)
  | not (any (\(a,b) -> a == x2) xs) =  [(x1,x2)]
  | otherwise = checkTuple' ((x1,x2):xs)
checkTuple' ((x1,x2):xs)
  | null xs = []
  | x2 == y1 = (x1,x2): (x1,y2) : checkTuple' ((x1,x2):[])
  | otherwise = checkTuple' ((x1,x2):[])
      where (y1,y2) = head xs

-- Taken from https://stackoverflow.com/questions/1041440/how-to-move-an-element-in-a-list-in-haskell
move :: Int -> [a] -> [a]
move n as = head ts : (hs ++ tail ts)
   where (hs, ts) = splitAt n as



composeR :: (Num a1, Eq a1, Eq a2) => [(a2, a2)] -> [(a2, a2)] -> a1 -> a1
composeR r1 r2 counter
  | counter == 0 = composeR r1 newr2 (counter+1)
  | r1 == r2 = counter
  | otherwise = composeR r1 newr2 (counter+1)
  where newr2 = concatMap (\(x,y) -> concatMap (\(x2, y2) -> [(x,y2) | y == x2]) r1) r2

