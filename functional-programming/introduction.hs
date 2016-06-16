-- Imports
import qualified Data.List as List

---------------------------------------------------------------------------------------------------

{-| Adds two numbers -}
solveMeFirst :: (Num a) => a -> a -> a
solveMeFirst a b = a + b
---------------------------------------------------------------------------------------------------

{-| Hello World -}
hello :: String
hello = "Hello World"
---------------------------------------------------------------------------------------------------

{-| Hello World given number of times -}
hellos :: Int -> [String]
hellos n = replicate n hello
---------------------------------------------------------------------------------------------------

{-| Repeat each element of a list n number of times -}
repeater :: Int -> [a] -> [[a]]
repeater n = map (replicate n)
---------------------------------------------------------------------------------------------------

{-| Filter out all elems in a list that are greater than n -}
filterArray :: (Ord a) => a -> [a] -> [a]
filterArray n = filter (< n)
---------------------------------------------------------------------------------------------------

{-| Filter out all elems on odd positions in a list. -}
filterOdd :: [a] -> [a]
filterOdd [] = []
filterOdd [x] = []
filterOdd (x1:x2:xs) = x2 : filterOdd xs
---------------------------------------------------------------------------------------------------

{-| Create a list of size n -}
createList :: Int -> [Int]
createList n = [1..n]
---------------------------------------------------------------------------------------------------

{-| Ineffeciently reverse a list -}
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
---------------------------------------------------------------------------------------------------

{-| Sum of all the odd integers -}
sumOdds :: [Integer] -> Integer
sumOdds = sum . (filter (\x -> x `mod` 2 == 1))
---------------------------------------------------------------------------------------------------

{-| length of a list -}
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs
---------------------------------------------------------------------------------------------------

{-| absoultify the list (i.e make everything absolute) -}
absolutify :: [Integer] -> [Integer]
absolutify = map abs
---------------------------------------------------------------------------------------------------

{-| calculate e^x i.e. 1 + e + e^2/2! + e^3/3! + ..... -}
powere :: Double -> Double
powere x = sum (map (\y -> x**y / factorial y) [0..9])
    where factorial 0 = 1
          factorial n = n * factorial (n - 1)
---------------------------------------------------------------------------------------------------

{-|
 - This function should return a list [area, volume].
 - For the purpose of numerical computation, the area under the curve  between the limits a
 - and b can be computed by the Limit Definition of a Definite Integral.
 -
 - Using equal subintervals of length = 0.001: 
 - 1. Evaluates the area bounded by a given polynomial function of the kind described above,
 -    between the given limits of l and r. (solveArea)
 - 2. Evaluates the volume of the solid obtained by revolving this polynomial curve around
 -    the x-axis. (solveVol)
 -
 - y: Get the value of coordinate y given a, b, x coordinate using
 -    (a_1 * x^b_1 + .... + a_n * x^b_n)
 - where length a == length b
 -}
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b =
  [ (solveArea (fromIntegral l) (fromIntegral r) a b) / 1000
  , (solveVol (fromIntegral l) (fromIntegral r) a b) / 1000
  ]
  where
    y [] [] x         = 0.0
    y (a:as) (b:bs) x = (fromIntegral a) * (x^^b) + y as bs x
    solveArea l r a b = foldl (+) 0.0 (map (y a b) [l,l+0.001..r])
    solveVol l r a b  = foldl (+) 0.0 (map (\x -> pi * (y a b x)^2) [l,l+0.001..r])
---------------------------------------------------------------------------------------------------

-- LAMBDA CALCULS: REDUCTIONS
---------------------------------------------------------------------------------------------------
-- ((λx.(x y))(λz.z)) = y
---------------------------------------------------------------------------------------------------
-- ((λx.((λy.(x y))x))(λz.w)) = w
---------------------------------------------------------------------------------------------------
-- ((λx.(x x))(λx.(x x))) = CAN'T REDUCE
---------------------------------------------------------------------------------------------------
-- (λg.((λf.((λx.(f (x x)))(λx.(f (x x))))) g)) = CAN'T REDUCE
---------------------------------------------------------------------------------------------------

-- LAMBDA CALCULS: EVALUATING EXPRESSIONS
---------------------------------------------------------------------------------------------------
-- (λx.x+1)3 = 4
---------------------------------------------------------------------------------------------------
-- (λx.x+1)((λy.y+2)3) = 6
---------------------------------------------------------------------------------------------------
-- λx.λy.x^(47)y = 47
---------------------------------------------------------------------------------------------------
-- λx.λy.x(xy) = 2
---------------------------------------------------------------------------------------------------
-- λx.λy.y = 0
---------------------------------------------------------------------------------------------------

{-|
 - Checks if a given list of values i.e. x-coord and y-coord are the input and output
 - respectively of a mathemetical function
 -}
isFunction :: (Ord a, Ord b, Num a, Num b) => [(a, b)] -> [Char]
isFunction xs = duplicateY (List.sort xs) (501, 501)
  where
    -- x_1 == x_0 and y_1 /= y_0
    -- => two values for same input
    -- => Not a function in a mathematical sense
    duplicateY [] tup = "YES"
    duplicateY (x:xs) tup
      | fst x == fst tup && snd x /= snd tup = "NO"
      | otherwise = duplicateY xs x

---------------------------------------------------------------------------------------------------

{-|
 - perimeter of the ploygon whose verticies are given in order
 -}
perimeter :: (Integral a, Integral b, Floating c) => [(a, b)] -> c
perimeter lst = foldl (+) 0.0 (zipWith distance lst (tail (cycle lst)))
  where
    distance (x1, y1) (x2, y2) = sqrt ((fromIntegral (x2 - x1))^2 + (fromIntegral (y2 - y1))^2)
---------------------------------------------------------------------------------------------------

{-|
 - Area of the ploygon whose verticies are given in order
 -}
area :: (Integral a, Fractional b) => [(a, a)] -> b
area lst = abs (foldl (+) 0.0 (zipWith xysum lst (tail (cycle lst))))
  where
    xysum (x1, y1) (x2, y2) = (fromIntegral ((x1 * y2) - (x2 * y1))) / 2
---------------------------------------------------------------------------------------------------
 
