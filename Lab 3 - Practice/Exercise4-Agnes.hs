import Test.QuickCheck
import Lecture3

-- https://www.stackbuilders.com/blog/a-quickcheck-tutorial-generators/
--instance Arbitrary a => Arbitrary (Form f) where
  --arbitrary =
    --sized 
    

--arbitrarySizedForm :: Arbitrary f => Int -> Gen (Form f)
--arbitrarySizedForm m = do
  --t <- arbitrary
  --n <- choose (0, m `div` 2)
  --ts <- vectorOf n (arbitrarySizedForm (m `div` 4))
  --return (Form t ts)

  -- genLongNegRange = (arbitrary :: Gen Int) `suchThat` (\x -> (x < (-10)) && (x > (-15)))

genFormula :: Gen 
genFormula = arbitrary :: Gen String a 