module Exercise4 where

import Data.List
import SetOrd
import System.Random
import Test.QuickCheck

-- Time spend: 90 minutes --

-- =================================== DOCUMENTATION OF APPROACH ===================================
--

-- =================================== IMPLEMENTATION ===================================
type Rel a = [(a, a)]

isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial domain relation = isSerialHelper domain domain relation

isSerialHelper [] _ _ = True
isSerialHelper (x : xs) domain relation = if secondHelper x domain relation then isSerialHelper xs domain relation else False

-- =================================== TEST ===================================
-- Property that checks if the length of relations is smaller than the domain.
-- If it is smaller then that means that they are not serial.
correctLength :: Eq a => [a] -> Rel a -> Bool
correctLength domain relations = length relations < length domain && not (isSerial domain relations)

-- =================================== DISCUSSION ===================================

-- =================================== HELPERS ===================================
infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q