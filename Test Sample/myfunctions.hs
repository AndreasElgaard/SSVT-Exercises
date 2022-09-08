import Test.QuickCheck

doubleMe x = x + x  
doubleUs x y = (x*2) + (y*2)

doubleSmallNumber x = if (x >= 100) 
    then x
    else x * 2
    
fourExample = "four" ++ "tat"

removeNonUppercase :: [Char] -> [Char]  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   

removeNonLowercase :: String -> String 
removeNonLowercase st = [ c | c <- st, c `elem` ['a', 'c', ' ']]   

lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"  

sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"  

-- bmiTell :: Float -> String  
-- bmiTell bmi  
--     | bmi <= 18.5 = "You're underweight, you emo, you!"  
--     | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
--     | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
--     | otherwise   = "You're a whale, congratulations!"  

bmiTell :: Float -> Float -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  

bmiTell' :: Float -> Float -> Float  
bmiTell' weight height  
    | bmi <= skinny = bmi
    | bmi <= normal = bmi 
    | bmi <= fat    = bmi
    | otherwise     = bmi
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  

initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname  

initials' :: [Char] -> [Char] -> String 
initials' (x:_) (y:_) = [x] ++ ". " ++ [y]

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  

calcBmis'' :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis'' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]  

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis' xs = [w/h^2 | (w, h) <- xs]  


testBmi :: Float -> Float -> Float  
testBmi weight height = weight / height * 2

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  
    
cylinder' :: (RealFloat a) => a -> a -> a  
cylinder' r h = sideArea + 2 * topArea  
    where sideArea = 2 * pi * r * h  
          topArea = pi * r ^2  


replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x  

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

quicksort' :: (Ord a) => [a] -> [a]  
quicksort' [] = []  
quicksort' (x:xs) = smallerSorted ++ [x] ++ biggerSorted    
    where smallerSorted = quicksort [a | a <- xs, a <= x]  
          biggerSorted = quicksort [a | a <- xs, a > x]  

multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z  

multThreeTest :: Integer -> Integer -> Integer -> Bool 
multThreeTest x y z = x * y * z < 1000

divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)  

prop_commutativeAdd :: Integer -> Integer -> Bool
prop_commutativeAdd n m = n + m == m + n

reversal :: Integer -> [Char]
reversal = reverse . show