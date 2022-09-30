module Exercise6 where

import Data.List
import LTS
import System.Random
import Test.QuickCheck

-- Time spend: 180 minutes --

-- TASK AT HAND:
-- Implement a function that specifies the correct behavior for a door implementation.

-- =================================== Implementation ===================================
-- We wanted to create a function: 
testLTSAgainstSUT :: IOLTS -> (State -> Label -> (State, Label)) -> Bool
testLTSAgainstSUT = undefined
-- that takes an IOLTS which we define like: 
doorIOLTS = createIOLTS [(0, "?close", 1), (1, "!closed", 2), (1, "?open", 0), (1, "!opened", 0)] -- and so on
-- and the SUT as the second parameter. 

-- A way to check if the SUT is correctly implemented is to use the straces function from exercise3.
-- By using this function we could compare the two [Traces] to see if they are equal. 

-- =================================== Description of Each Bug ===================================
-- doorImpl1
-- Correct. No bugs.

-- doorImpl2
-- When closing the door it transitions to the wrong label. It transitions to opened.
-- When opening the door it transitions to the wrong label. It transitions to closed.

-- doorImpl3
-- When unlocking the door transitions to the wrong state. It transitions to itself.

-- doorImpl4
-- When unlocking the door it transitions to locking the door
-- When locking the door it transitions to unlocking the door.

-- doorImpl5
-- When opening the door it transitions to the wrong label. It transitions to open.

-- doorImpl6
-- Few things wrong with this one. For example when the door tries to open it says it is stuck.

-- doorImpl7
-- Few things wrong with this one. For example when the door is unlocked it says it is the wrong keycode.