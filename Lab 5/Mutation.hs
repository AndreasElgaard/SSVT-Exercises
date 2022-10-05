module Mutation where
import Test.QuickCheck
import Data.List
import Data.Maybe

mutate :: ([Integer] -> Gen [Integer]) -> ([Integer] -> Integer -> Bool) -> (Integer -> [Integer]) -> Integer -> Gen (Maybe Bool)
mutate mutator prop fut input = mutation >>= \mutant -> mutateOrNothing output mutant (Just <$> propertyExecutor prop mutation input)
        where output = fut input
              mutation = mutator output

mutateOrNothing :: [Integer] -> [Integer] -> Gen (Maybe Bool) -> Gen (Maybe Bool)
mutateOrNothing output mutant res | output == mutant = return Nothing
                                  | otherwise = res

propertyExecutor :: ([Integer] -> Integer -> Bool) -> Gen [Integer] -> Integer -> Gen Bool
propertyExecutor prop o x = o >>= \output -> return $ prop output x

-- Mutators
addElements :: [Integer] -> Gen [Integer]
addElements xs = do
  nums <- arbitrary :: Gen [Integer]
  num <- arbitrary :: Gen Integer
  return $ num : xs ++ nums

removeElements :: [Integer] -> Gen [Integer]
removeElements xs = choose (1, length xs - 1) >>= \x -> return $ take x xs
