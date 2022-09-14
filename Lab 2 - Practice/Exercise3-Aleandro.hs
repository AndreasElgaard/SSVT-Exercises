module Exercise3 where
import Data.List
import Data.Char
-- import System.Random
import Test.QuickCheck

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

-- The stronger function doesnt work for me yet.. forall isnt found and i cant find which package its imported from
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

-- prop_one :: Int -> Bool
-- prop_one x =  (\x -> even x && x > 3) x || even x

-- Each proposition was split into the left and right side to be able to test their strength / weakness independantly 
prop_oneL :: Int -> Bool
prop_oneL x =  even x && x > 3 || even x

prop_oneR :: Int -> Bool
prop_oneR = even

-- prop_two :: Int -> Bool
-- prop_two x = (even x || x > 3)  || even x

prop_twoL :: Int -> Bool
prop_twoL x = (even x || x > 3)  || even x

prop_twoR :: Int -> Bool
prop_twoR = even

-- prop_three :: Int -> Bool
-- prop_three x = ((even x && x > 3)|| even x) || even x

prop_threeL :: Int -> Bool
prop_threeL x = (even x && x > 3)|| even x

prop_threeR :: Int -> Bool
prop_threeR = even

-- prop_four :: Int -> Bool
-- prop_four x = even x || ((even x && x > 3) || even x)

prop_fourL :: Int -> Bool
prop_fourL = even

prop_fourR :: Int -> Bool
prop_fourR x =  (even x && x > 3) || even x

-- Example stronger [(-10)..10] prop_oneL prop_oneR

-- TODO Add functions to rank properties