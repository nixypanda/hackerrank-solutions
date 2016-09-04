{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Introduction
  ( Point
  , Vector
  , area
  , distance
  , factorial
  , perimeter
  , vcross
  , introduction
  ) where


-- Imports
import Control.Monad (replicateM, forM_)

import qualified Data.List as List


-- HELPERS ---------------------------------------------------------------------------------------


{-| point data type representing a point on a 2-d cartesian coordiante system -}
type Point =
  (Double, Double)


{-| A Vector data type -}
type Vector =
  (Double, Double)


{-|
 - Get cartesian distance between two points on x-y plane
 -}
distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) =
  sqrt $ (x2 - x1)**2 + (y2 - y1)**2


{-|
 - Get the factorial of a number
 -}
factorial :: Integer -> Integer
factorial n =
  product [1..n]


{-| Cross product of two vectors -}
vcross :: Vector -> Vector -> Double
vcross (x1, y1) (x2, y2) =
  x1 * y2 - y1 * x2


-- SOLVE ME FIRST FP ------------------------------------------------------------------------------

{-| Adds two numbers -}
solveMeFirst :: Int -> Int -> Int
solveMeFirst a b =
  a + b

mainSolveMeFirst :: IO ()
mainSolveMeFirst = do
  value <- solveMeFirst <$> readLn <*> readLn
  print value


-- HELLO WORLD ------------------------------------------------------------------------------------

{-| Hello World -}
hello :: String
hello =
  "Hello World"

mainHello :: IO ()
mainHello =
  print hello


-- HELLO WORLD N TIMES ----------------------------------------------------------------------------

{-| Hello World given number of times -}
hellos :: Int -> [String]
hellos n =
  replicate n hello

mainHellos :: IO ()
mainHellos = do
  n <- readLn :: IO Int
  putStr . unlines $ hellos n


-- LIST REPLICATION -------------------------------------------------------------------------------

{-| Repeat each element of a list n number of times -}
repeater :: Int -> [a] -> [[a]]
repeater n =
  map (replicate n)

mainRepeater :: IO ()
mainRepeater =
  getContents >>= mapM_ print . (\(n:arr) -> repeater n arr) . map read . words


-- FILTER ARRAY -----------------------------------------------------------------------------------

{-| Filter out all elems in a list that are greater than n -}
filterArray :: (Ord a) => a -> [a] -> [a]
filterArray n =
  filter (< n) 


mainFilterArray :: IO ()
mainFilterArray =
  getContents >>= mapM_ print . (\(n:as) -> filterArray n as) . map (read :: String -> Int) . words


-- FILTER POSITIONS IN A LIST ---------------------------------------------------------------------

{-| Filter out all elems on odd positions in a list. -}
filterOdd :: [a] -> [a]
filterOdd [] = []
filterOdd [_] = []
filterOdd (_ : x2 : xs) =
  x2 : filterOdd xs

mainFilterOdd :: IO ()
mainFilterOdd =
  getContents >>= mapM_ print . filterOdd . map (read :: String -> Int) . words


-- CREATE LIST ------------------------------------------------------------------------------------

{-| Create a list of size n -}
_createList :: Int -> [Int]
_createList n =
  [1..n]


-- REVERSE A LIST ---------------------------------------------------------------------------------

{-| Reverse a list -}
_reverse' :: [a] -> [a]
_reverse' =
  foldl (flip (:)) []


-- SUM OF ODD ELEMENTS ----------------------------------------------------------------------------

{-| Sum of all the odd integers -}
sumOdds :: [Integer] -> Integer
sumOdds =
  sum . filter (\x -> x `mod` 2 == 1)


mainSumOdds :: IO ()
mainSumOdds =
  getContents >>= print . sumOdds . map read . lines


-- LIST LENGTH ------------------------------------------------------------------------------------

{-| length of a list -}
_length' :: [a] -> Int
_length' =
  foldr (\_ -> (+) 1) 0


-- UPDATE LIST ------------------------------------------------------------------------------------

{-| absoultify the list (i.e make everything absolute) -}
absolutify :: [Integer] -> [Integer]
absolutify =
  map abs

mainAbsolutify :: IO ()
mainAbsolutify =
  getContents >>= mapM_ print . absolutify . map (read :: String -> Integer) . lines


-- EVALUATING e^x ---------------------------------------------------------------------------------

{-|
 - calculate c^x i.e. 1 + c + c^2/2! + c^3/3! + .....
 - where c is the supplied number
 -}
powere :: Double -> Double
powere x =
  sum $ map (\y -> x^^y / fromIntegral (factorial y)) [0..9]

mainPowere :: IO ()
mainPowere =
  getContents >>= mapM_ (print . powere . (read :: String -> Double)) . tail . words


-- AREA UNDER CURVE AND VOLUME OF REVOLVING A CURVE -----------------------------------------------

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
curve :: Int -> Int -> [Int] -> [Int] -> [Double]
curve left right ps qs =
  [ solveArea (fromIntegral left) (fromIntegral right) ps qs / 1000
  , solveVol (fromIntegral left) (fromIntegral right) ps qs / 1000
  ]
  where
    -- gives the y coordinate for a given x according to the given equation
    y :: [Int] -> [Int] -> Double -> Double
    y [] [] _ = 0.0
    y [] _ _ = error "Length of provided lists is not equal"
    y _ [] _ = error "Length of provided lists is not equal"
    y (a:as) (b:bs) x =
      fromIntegral a * (x^^b) + y as bs x

    solveArea :: Double -> Double -> [Int] -> [Int] -> Double
    solveArea l r a b =
      sum $ map (y a b) [l, l + 0.001..r]

    solveVol :: Double -> Double -> [Int] -> [Int] -> Double
    solveVol l r a b =
      sum $ map (\x -> pi *  y a b x ** 2) [l, l + 0.001..r]


mainCurve :: IO ()
mainCurve =
  getContents >>= mapM_ print . (\[a, b, [l, r]] -> curve l r a b) . map (map read . words) . lines


-- LAMBDA CALCULS: REDUCTIONS ---------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- ((λx.(x y))(λz.z)) = y
---------------------------------------------------------------------------------------------------
-- ((λx.((λy.(x y))x))(λz.w)) = w
---------------------------------------------------------------------------------------------------
-- ((λx.(x x))(λx.(x x))) = CAN'T REDUCE
---------------------------------------------------------------------------------------------------
-- (λg.((λf.((λx.(f (x x)))(λx.(f (x x))))) g)) = CAN'T REDUCE
---------------------------------------------------------------------------------------------------


-- LAMBDA CALCULS: EVALUATING EXPRESSIONS ---------------------------------------------------------
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


-- FUNCTIONS OR NOT? ------------------------------------------------------------------------------

{-|
 - Checks if a given list of values i.e. x-coord and y-coord are the input and output
 - respectively of a mathemetical function
 -}
isFunc :: [Point] -> String
isFunc ys =
  duplicateY (List.sort ys) (501, 501)
    where
      -- x_1 == x_0 and y_1 /= y_0
      -- => two values for same input
      -- => Not a function in a mathematical sense
      duplicateY [] _ = "YES"
      duplicateY (x:xs) tup =
        if fst x == fst tup && snd x /= snd tup then "NO" else duplicateY xs x

mainIsFunc :: IO ()
mainIsFunc = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \_ -> do
    n <- readLn :: IO Int
    replicateM n getLine >>= putStrLn . isFunc . map ((\[a,b] -> (a, b)) . map read . words)


-- COMPUTE THE PERIMETER OF THE POLYGON -----------------------------------------------------------

{-|
 - Perimeter of the ploygon whose verticies are given in clockwise / counter clockwise order.
 - given a list of points pair them with the point immediatly next to them
 - then find the distance between 2 and sum over all
 -}
perimeter :: [Point] -> Double
perimeter lst =
  sum $ zipWith distance lst (tail $ cycle lst)

mainPerimeter :: IO ()
mainPerimeter =
  getContents >>= print . perimeter . map ((\[x,y] -> (x,y)) . map read . words) . tail . lines


-- COMPUTE THE AREA OF THE POLYGON ----------------------------------------------------------------

{-|
 - Area of the ploygon whose verticies are given in order.
 -
 - Cross product gives the area of the parlleogram formed by the two vectors
 - so taking half of it will give the signed area of the triangle formed by a, b and (b - a)
 - where a and b are vectors
 - P.S. you can think of Points as vectors.
 -}
area :: [Point] -> Double
area lst =
  abs $ sum $ zipWith (\v1 v2 -> (v1 `vcross` v2) / 2) lst (tail $ cycle lst)

mainArea :: IO ()
mainArea =
  getContents >>= print . area . map ((\[x,y] -> (x,y)) . map read . words) . tail . lines


---------------------------------------------------------------------------------------------------


introduction :: [(String, IO ())]  
introduction =
  [ ("solveMeFirst", mainSolveMeFirst)  
  , ("hello", mainHello)
  , ("hellos", mainHellos)
  , ("repeater", mainRepeater)
  , ("filterArray", mainFilterArray)
  , ("filterOdd", mainFilterOdd)
  , ("sumOdds", mainSumOdds)
  , ("absoultify", mainAbsolutify)
  , ("powere", mainPowere)
  , ("curve", mainCurve)
  , ("isFunc", mainIsFunc)
  , ("perimeter", mainPerimeter)
  , ("area", mainArea)
  ]  

