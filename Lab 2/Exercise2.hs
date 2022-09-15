module Exercise2 where
import Test.QuickCheck

data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)

checkTriangle :: (Int, Int, Int) -> IO ()
checkTriangle (a, b, c) = 
    putStrLn (outputTriangleInfo shape)
    where shape = triangle a b c

outputTriangleInfo shape = case shape of
    NoTriangle ->  "Not a triangle (Geen driehoek)"
    Equilateral ->  "Equilateral (Gelijkzijdig)"
    Isosceles ->  "Isosceles (Gelijkbenig)"
    Other ->  "Other (Anders)"
    Rectangular -> "Rectangular (Rechthoekig)"

triangle :: Int -> Int -> Int -> Shape
triangle x y z
    -- We check that, if either value of a triangle side is <= 0, we do not consider the combination
    -- to form up a triangle, since a side always has to have a positive-value length.
    -- Furthermore, for the triangle to be a valid one, the lentgth of 2 sides > length of other side,
    -- hence the check whether all combinations of 2 sides exceed the length of other side.
    -- Refer to:  https://sciencing.com/rules-length-triangle-sides-8606207 Triangle Inequality theorem 1
    | not ((x + y) > z && (x + z) > y &&(z + y) > x) || x <= 0 || y <= 0 || z <= 0 = NoTriangle
    -- Checking that all sides are of equal length
    | x == y && y == z && x == z = Equilateral
    -- By Pytaghoras' theorem, in right-angled triangles, c^2 = a^2 + b^2
    | x^2 == y^2 + z^2 || y^2 == x^2 + z^2 || z^2 == x^2 + y^2 = Rectangular
    -- Checking that at least 2 sides are of equal length
    | x == y || y == x  || z == x = Isosceles
    | otherwise = Other

-- For a valid triangle, each side should have a positive length.
genLength :: Gen Int 
genLength = (arbitrary :: Gen Int) `suchThat` (>0)

-- Confirming triangle inequality theorem 1 for valid triangle
prop_triangleSumTwoSideLength :: Int -> Int -> Int -> Property
prop_triangleSumTwoSideLength x y z =
    (triangle x y z /= NoTriangle) ==> (x + y) > z && (x + z) > y &&(z + y) > x

-- Confirming triangle is equilateral when same valid value (>0) is passed for 3 arguments
prop_equilateralTriangle :: Int -> Property
prop_equilateralTriangle x = triangle x x x/= NoTriangle ==> triangle x x x == Equilateral

-- Confirming triangle is isoscles when same valid value (>0) is passed for 2 arguments
prop_isoscelesTriangle :: Int -> Int -> Property
prop_isoscelesTriangle x y = (x /= y && triangle x x y/= NoTriangle) ==> triangle x x y == Isosceles

-- prop_rectangularTriangle :: Int -> Int -> Property
-- prop_rectangularTriangle x y =
--     (x /= y && triangle x y (sqrt((x^2 + y^2)))/= NoTriangle) ==> triangle x y (x^2 + y^2) == Rectangular

-- Test Report 
main :: IO ()
main = do
    putStrLn "\n=== Testing length of any 2 sides of triangle > length of other side ===\n"
    quickCheck $ forAll genLength $ \x-> forAll genLength$ \y -> forAll genLength$ \z ->
        prop_triangleSumTwoSideLength x y z
    putStrLn "\n=== Testing 3 equal sided triangle is Equilateral ===\n"
    quickCheck $ forAll genLength prop_equilateralTriangle
    putStrLn "\n=== Testing 2 equal sided triangle is Isosceles ===\n"
    quickCheck $ forAll genLength $ \x-> forAll genLength$ \y -> prop_isoscelesTriangle x y
    -- putStrLn "\n=== Testing triangle compliant with Pythagoras' Theorem is Rectangular ===\n"
    -- quickCheck $ forAll genLength $ \x-> forAll genLength$ \y -> prop_rectangularTriangle x y



