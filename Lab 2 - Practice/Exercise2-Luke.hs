module Exercise2 where
import Test.QuickCheck

data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle' x y z = ((x + y) > z && (x + z) > y &&(z + y) > x)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z 
    | not ((x + y) > z && (x + z) > y &&(z + y) > x) || x <= 0 || y <= 0 || z <= 0 = NoTriangle
    | x == y && y == z && x == z = Equilateral
    | x^2 == y^2 + z^2 || y^2 == x^2 + z^2 || z^2 == x^2 + y^2 = Rectangular
    | x == y || y == x  || z == x = Isosceles
    | otherwise = Other


prop_triangleSumTwoSideLength x y z =
    (triangle x y z /= NoTriangle) ==> (x + y) > z && (x + z) > y &&(z + y) > x

-- Test Report 
main :: IO ()
main = do
    putStrLn "\n=== Testing length of any 2 sides of triangle > length of other side ===\n"
    quickCheck prop_triangleSumTwoSideLength