module Exercise5 where
import           Data.List                      ( elemIndex
                                                , nub
                                                )
import Exercise3 ( Rel )
-- Time spent: 200 minutes

-- =================== Documentation of thinking process ==================== --
-- Transitive closure iterates through each tuple in a relation. The first tuple (x1,y1) is added
-- to the output of the transitive closure, the second element of the tuple is then searched
-- in the first element of the  proceeding tuples (x2,y2). If the second elemnt of the first tuple (y1)
-- and the second element of the current iteratations (x1) tuple are equal the following is
-- added to the output list (x1, y2). Relations must only contain unique tuples, therefor any duplcates
-- are removed. When the search is complete for one tuple, the same process is replicated for the next tuple.
trClos :: Eq a => Rel a -> Rel a
trClos rels = nub (iterateWIndex 0 rels)

-- Iterates over the provided list of relations
--    If the counter+1 is equal to the length of the list, the final run is made
--    Otherwise, Run check tuple for the current full list, the head of the list is assumed to be
--    the tuple which is currently being processed
iterateWIndex :: Eq a => Int -> Rel a -> Rel a
iterateWIndex counter [] = []
iterateWIndex counter xy@(x : xs)
  | (counter + 1) == length xy = checkTuple' xy
  | otherwise =  checkTuple' xy
  ++ iterateWIndex (counter + 1) (move (counter + 1) xy)

-- This function first pattern matches with the base case
-- The second pattern compares the first and second tuples, if the second param of the
--    first tuple and the first param of the second tuple are the same, the first tuple
--    and the combined tuple are appended to the result list
-- The next iterations of the tuple are processed by calling the function recursilvey with
--    the new processed tuple
-- If second param is not found in the list, only the head is appened to the list
-- Otherwise the funciton is called with the tail of the inputted list
checkTuple' :: Eq a => Rel a -> Rel a
checkTuple' [] = []
checkTuple' ((x1, x2) : (y1, y2) : xs)
  | x2 == y1 = (x1, x2) : (x1, y2) : checkTuple' ((x1, x2) : xs) ++ checkTuple'
    ((x1, y2) : xs)
  | not (any (\(a, b) -> a == x2) xs) = [(x1, x2)]
  | otherwise = checkTuple' ((x1, x2) : xs)
checkTuple' ((x1, x2) : xs)
  | null xs   = []
  | x2 == y1  = (x1, x2) : (x1, y2) : checkTuple' ((x1, x2) : [])
  | otherwise = checkTuple' ((x1, x2) : [])
  where (y1, y2) = head xs

-- Taken from https://stackoverflow.com/questions/1041440/how-to-move-an-element-in-a-list-in-haskell
-- This function moves a given index to the head of the list, this is used to make it easier
-- to search through the list of tuples
move :: Int -> [a] -> [a]
move n as = head ts : (hs ++ tail ts) where (hs, ts) = splitAt n as

-- ============ Alternative we tried to implement with fix ====================== --
infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x, z) | (x, y) <- r, (w, z) <- s, y == w ]

-- trClos :: Eq a => Rel a -> Rel a
-- trClos rels = nub (iterateWIndex 0 rels)

-- We wanted to use fix for this but couldnt make it work properly, so we have a solution
-- that is make from scratch, a implemention but it works as intended B)
-- fix :: (a -> a) -> a
-- fix f = f (fix f)
-- infixl 1 $$

-- ($$) :: a -> (a -> b) -> b
-- ($$) = flip ($)
-- test :: Rel a -> Rel a
-- test rels = (0, rels, []) $$
--   fix (\ f (counter, relList,outPut) -> if (counter+1) == length then outPut else f (counter+1, tail relList, (relList !! counter) @@ relList))





