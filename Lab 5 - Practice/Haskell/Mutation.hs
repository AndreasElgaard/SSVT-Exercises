module Mutation where
import           Data.List
import           Data.Maybe
import           Debug.Trace
import           Test.QuickCheck

-- Applies a mutator to a property and function under test, then returns whether the mutant is killed (Just False), whether it lives (Just True), or that the mutant did not change the output (Nothing)
mutate
  :: Eq a
  => (a -> Gen a)
  -> (a -> Integer -> Bool)
  -> (Integer -> a)
  -> Integer
  -> Gen (Maybe Bool)
mutate mutator prop fut input = mutation >>= \mutant -> mutateOrNothing
  output
  mutant
  (Just <$> propertyExecutor prop mutation input)
 where
  output   = fut input
  mutation = mutator output

-- Returns the mutated result, or nothing if the result is identical to the original output
mutateOrNothing :: Eq a => a -> a -> Gen (Maybe Bool) -> Gen (Maybe Bool)
mutateOrNothing output mutant res | output == mutant = return Nothing
                                  | otherwise        = res

propertyExecutor
  :: Eq a => (a -> Integer -> Bool) -> Gen a -> Integer -> Gen Bool
propertyExecutor prop o x = o >>= \output -> return $ prop output x


-- Applies a mutator to a property and function under test, then returns whether the mutant is killed (False), whether it lives (True), or that the mutant did not change the output (empty list)
mutate'
  :: Eq a
  => (a -> Gen a)
  -> [a -> Integer -> Bool]
  -> (Integer -> a)
  -> Integer
  -> Gen [Bool]
mutate' mutator prop fut input = mutation >>= \mutant ->
  mutateOrNothing' output mutant (propertyExecutor' prop mutation input)
 where
  output   = fut input
  mutation = mutator output

propertyExecutor'
  :: Eq a => [a -> Integer -> Bool] -> Gen a -> Integer -> Gen [Bool]
propertyExecutor' prop o x =
  o >>= \output -> return $ map (\y -> y output x) prop

-- Returns the mutated result, or nothing if the result is identical to the original output
mutateOrNothing' :: Eq a => a -> a -> Gen [Bool] -> Gen [Bool]
mutateOrNothing' output mutant res | output == mutant = return []
                                   | otherwise        = res

-- == Mutators ==
-- Adds elements to the beginning and/or end of an output list
addElements :: [Integer] -> Gen [Integer]
addElements xs = do
  nums  <- arbitrary :: Gen [Integer]
  nums2 <- arbitrary :: Gen [Integer]
  return $ nums ++ xs ++ nums

-- Removes 1 to (length - 1) elements from an output list
removeElements :: [Integer] -> Gen [Integer]
removeElements xs = choose (1, length xs) >>= \x -> return $ take x xs

-- Any list
anyList :: [Integer] -> Gen [Integer]
anyList xs = arbitrary

mutators = [anyList, removeElements, addElements]


-- Example used in the lecture generate $
--  generate $ sequence $ take 20 $ repeat $ mutate removeElements prop_tenElements multiplicationTable  1
-- Conjecture -> Disjunciotn , Equivalence, FitSpeck talks about some of them
