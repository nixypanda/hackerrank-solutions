{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Recursion
  ( fibonacci
  , recursion
  ) where

import Control.Monad
  ( forM_
  , replicateM_
  ) 
import Data.List

import Data.Ord (comparing)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Char as Char

import Introduction
  ( Point
  , perimeter
  , vcross
  )


-- COMPUTING THE GCD ------------------------------------------------------------------------------

{-| Compoute the greatest common divisor of two numbers -}
gcd' :: (Integral a) => a -> a -> a
gcd' m n
  | m > n = gcd' (m - n) n
  | m < n = gcd' (n - m) m
  | otherwise = n

mainGCD :: IO ()
mainGCD =
  getLine >>= print . uncurry gcd' . (\[x, y] -> (x, y)) . map (read :: String -> Int) . words


-- FIBONACCI NUMBERS ------------------------------------------------------------------------------

{-|
 - Infinite list of fibonacci numbers. To get nth just do fibonacci !! n
 -}
fibonacci :: [Integer]
fibonacci =
  0 : 1 : zipWith (+) fibonacci (tail fibonacci)

mainFibonacci :: IO ()
mainFibonacci =
  readLn >>= (\n -> print $ fibonacci !! n)


-- PASCAL'S TRIANGLE ------------------------------------------------------------------------------

{-|
 - Returns an infinite list of pascal triangle rows.
 -}
pascal :: [[Integer]]
pascal =
  map init nCr
    where
      nVec :: [Integer] -> [Integer]
      nVec [] = error "This cannot happen"
      nVec [0] = [0]
      nVec [_] = error "This cannot happen"
      nVec (y1 : y2 : ys) =
        (y1 + y2) : nVec (y2 : ys)

      nCr :: [[Integer]]
      nCr = [1, 0] : [1, 1, 0] : map (\x -> 1 : nVec x) (tail nCr)


mainPascal :: IO ()
mainPascal =
  readLn >>= (\n -> putStr . unlines . map (unwords . map show) $ take n pascal)


-- STRING MINGLING --------------------------------------------------------------------------------

{-| mingle together multiple strings -}
mingle :: [a] -> [a] -> [a]
mingle [] [] = []
mingle [] _ = error "Well now the strings are not of same length"
mingle _ [] = error "Well now the strings are not of same length"
mingle (x:xs) (y:ys) =
  x : y : mingle xs ys

mainMingle :: IO ()
mainMingle =
  mingle <$> getLine <*> getLine >>= putStrLn


-- STRING-O-PERMUTE -------------------------------------------------------------------------------

{-| swap numbers at even odd -}
swap :: [a] -> [a]
swap [] = []
swap [_] = error "You know swaping even odd positions is hard on odd length strings!!"
swap (x1:x2:xs) =
  x2 : x1 : swap xs

mainSwap :: IO ()
mainSwap =
  readLn >>= (\t -> replicateM_ t (getLine >>= putStrLn . swap))


-- STRING REDUCTIONS ------------------------------------------------------------------------------

{-|
 - Filters out all the repeated elements while preserving the order in which the elements first
 - appear unique elements of a list
 -}
reduction :: (Ord a) => Set.Set a -> [a] -> [a]
reduction _ [] = []
reduction seen (x:xs) =
  if x `Set.member` seen
    then reduction seen xs
    else x : reduction (x `Set.insert` seen) xs

mainReduction :: IO ()
mainReduction =
  getLine >>= putStrLn . reduction Set.empty


-- FILTER ELEMENTS --------------------------------------------------------------------------------

{-|
 - Only elements whose count is greater than k in the given list whilest preserving the first
 - occurance order.
 -}
filterGTk :: (Ord t, Num t) => t -> [t] -> [t]
filterGTk k xs =
  let
    mapF = Map.fromListWith (+) [(x, 1) | x <- xs]
    filterd = filter (\x -> maybe False (>= k) (Map.lookup x mapF)) xs
  in
    if null filterd
      then [ -1 ]
      else reduction Set.empty filterd

mainFilterGTk :: IO ()
mainFilterGTk = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \_ -> do
    [_,k] <- fmap (map read . words) getLine
    getLine >>= putStrLn . unwords . map show . filterGTk k . map (read :: String -> Int) . words


-- THE SUM OF POWERS ------------------------------------------------------------------------------

{-|
 - Returns the number of unique ways we can get n by adding kth powers of natural numbers.
 -}
sumOfPows :: Int -> Int -> Int
sumOfPows t k =
  length $ filter (\x -> sum x == t) $ pset t 0 $ takeWhile (<= t) $ map (^k) [1..]
    where
      -- returns the pset of xs | sum xs < t (reduces the search space by quite a lot)
      pset _ _ [] = [[]]
      pset n s (x:xs)
        | s + x > n = pset n s xs
        | otherwise = pset n s xs ++ map (x:) (pset n (s + x) xs)


mainSumOfPowers :: IO ()
mainSumOfPowers =
  sumOfPows <$> readLn <*> readLn >>= print


-- SUPER DIGIT ------------------------------------------------------------------------------------

{-|
 - Returns super digit of a number 
 - We define super digit of an integer x using the following rules:
 - 
 - Iff x has only 1 digit, then its super digit is x.
 - Otherwise, the super digit of x is equal to the super digit of the digit-sum of x. Here,
 - digit-sum of a number is defined as the sum of its digits.
 -}
superDigit :: String -> Int
superDigit [a] = Char.digitToInt a
superDigit xs =
  superDigit . show . sum $ map Char.digitToInt xs


mainSuperDigit :: IO ()
mainSuperDigit = do
  (n, k) <- fmap ((\[n, k] -> (n, read k)) . words) getLine
  print . superDigit . show $ k * superDigit n


-- FUNCTIONS AND FRACTALS: SIERPINSKI TRIANGLES ---------------------------------------------------

-- | appends and prepends n underbars to a given string
-- e.g. ubars 3 "aa"
-- ___aa___
ubars :: Int -> String -> String
ubars n lst =
  replicate n '_' ++ lst ++ replicate n '_'


{-| Constructs a trianle of 1's and underscores of the given height -}
-- n ones prepended to others which are prepended and appended with 1 '_'
-- e.g. triangle 5
-- __1__
-- _111_
-- 11111
triangle :: Int -> [String]
triangle 0 = []
triangle 1 = ["1"]
triangle n =
  replicate n '1' : map (ubars 1) (triangle (n - 2))


{-|
 - Generate the ever popular sierpinski triangle using '1' and '_' on 'rows' number of rows (should
 - be 2^k - 1 where k <- Natural numbers and 'n' recursive depth. more the n more preety the
 - pattern and will require more number of lines.
 -}
spnsk :: Int -> Int -> [String]
spnsk rows n
  -- 1 -> generate a triangle of ones with given number of rows
  | n == 1 =
    triangle (2 * rows + 1)
  -- construct bottom half by putting two of them (smaller) side-by-side
  -- construct top half by padding it (smaller) with '_'
  -- and just put them together
  | otherwise =
    zipWith (\x y -> x ++ '_' : y) smaller smaller ++ map (ubars (m + 1)) smaller

  where
    m  = rows `div` 2
    -- n -> generate pattern for n - 1: smaller
    smaller = spnsk m (n - 1)


{-|
 - Generate using '1' and '_' on 'rows' number of rows (should be 2^k - 1 where k <- Natural
 - numbers and 'n' recursive depth. more the n more preety the pattern and will require more
 - number of lines.
 -}
sierpinski :: Int -> Int -> String
sierpinski rows n =
  (unlines . reverse) $ spnsk rows (n + 1)


mainSierpinski :: IO ()
mainSierpinski =
  readLn >>= putStr . sierpinski 63


-- FUNCTIONS AND FRACTALS - RECURSIVE TREES -------------------------------------------------------

-- | creates n lines of a character appended and prepended with underscores
-- (like the bottom half of 'Y'
-- straight 3 'a'
-- ___a___
-- ___a___
-- ___a___
straight :: Int -> Char -> [String]
straight n ch =
  replicate n (ubars n [ch])


-- | creates slanting version (like the top half of 'Y')
-- e.g. slant 3 'a'
-- a_______a
-- _a_____a_
-- __a___a__
-- ___a_a___
slant :: Int -> Char -> [String]
slant 0 ch = [[ch, '_', ch]]
slant n ch =
  ([ch] ++ replicate (2 * n + 1) '_' ++ [ch]) : map (ubars 1) (slant (n - 1) ch)


-- | Creates a Y pattern with the given number of rows and a character
-- e.g. why 3 'a'
-- a_____a
-- _a___a_
-- __a_a__
-- ___a___
-- ___a___
why :: Int -> Char -> [String]
why r ch =
  slant (r - 1) ch ++ straight r ch

{-|
 - Creates the recursive tree drawing 
 -}
rTree :: Int -> Int -> Int -> [String]
rTree 1 n co
    | n >= co = why (16 `div` (2^n)) '_'
    | otherwise = why (16 `div` (2^n)) '1'

rTree n r co =
  map (ubar 1) (zip smaller smaller) ++ map (ubars (2^(n - 1) - 1)) (rTree 1 r co)
    where
      smaller       = rTree (n - 1) (r + 1) co
      ubar k (x, y) = x ++ replicate k '_' ++ y


-- | Modifies the tree generated by rTree to fit to the needs of the hacker-rank question
tree :: Int -> [String]
tree n =
  concat (replicate 100 "_") : map (init . ubars 19) (rTree 5 0 n)


mainTree :: IO ()
mainTree =
  readLn >>= putStr . unlines . tree


-- CONVEX HULL ------------------------------------------------------------------------------------

{-|
 - A Direction data type that describes the type of angle the intersectiono of two lines make
 - a _____b____d
 -        |
 -        |
 -        c
 - a -> b -> c is a clockwise turn
 - c -> b -> a is a counter-clockwise turn
 - a -> b -> d is a Straight line
 -}
data Direction
  = LeftTurn
  | RightTurn
  | Straight
  deriving (Read, Show, Eq)


{-|
 - Given three points in a 2-d space returns the direction of turn from parm1 -> param2 -> param3
 - INSIGHT: cross product of 2 vectors has a direction associated with it using the thumb rule one
 - can clearly see that left-turn => +ive direction on z axis and right-turn => -ive direction
 -}
turnType :: Point -> Point -> Point -> Direction
turnType (xa, ya) (xb, yb) (xc, yc)
  | crossProd < 0 = LeftTurn
  | crossProd > 0 = RightTurn
  | otherwise = Straight
  where
    -- calculate cross product of BA and BC
    crossProd =
      vcross (xa - xb, ya - yb) (xc - xb, yc - yb)


{-|
 - Given a point and a list of points sorts that list by comparing the polar angle that the points
 - make with the given refrence point.
 -}
sortByPolarAngle :: Point -> [Point] -> [Point]
sortByPolarAngle p =
  sortBy (comparing (polarAngle p))


{-|
 - Gives a tuple of the angle between two points and the absolute x distance between them
 -}
polarAngle :: Point -> Point -> (Double, Double)
polarAngle (x0, y0) (x, y) =
  (atan2 (y - y0) (x - x0), abs (x - x0))


{-|
 - Helper Functions:
 - This function when provided with a list of points sorted with respect to a refrence point
 - and singleton list containing the refrence point will return the convex hull for those set
 - of points in linear time.
 - 
 - Structure:
 - Angle from a -> b -> c is checked where b and c are in left Argument and a is in the right.
 - firstArgument keeps track of points to scan 
 - secondArgument keeps track of the convex hull constructed so far
 -
 - There are three cases to consider:
 - a -> b -> c makes a left turn then simply add b to the result
 - a -> b -> c makes a straight line then simple ignore b
 - a -> b -> c makes a right turn in addition to ignoring b we also need to see if a is valid
 - to do so we pop a from the convexHull and put it back into consideration
 -}
gScan :: [Point] -> [Point] -> [Point]
gScan [] [] = []
gScan _ [] = error "no minY but others passed"
gScan [] ys = ys
gScan [x] ys = x : ys
gScan (b : c : ps) (a : chs) =
  case turnType a b c of
    LeftTurn ->
      gScan (c : ps) (b : a : chs)

    Straight ->
      gScan (c : ps) (a : chs)

    RightTurn ->
      gScan (a : c : ps) chs


{-|
 - Gives the convex Hull of the given set of points in linear-arithmatic time.
 -}
convexHull :: [Point] -> [Point]
convexHull xs =
  let
    minY = minimumBy (comparing snd) xs
  in
    gScan (sortByPolarAngle minY xs ++ [minY]) [minY]


mainConvexHull :: IO ()
mainConvexHull =
  getContents >>=
    print . perimeter . convexHull . map ((\[x,y] -> (x,y)) . map read . words) . tail . lines


---------------------------------------------------------------------------------------------------

recursion :: [(String, IO ())]
recursion =
  [ ("gcd", mainGCD)
  , ("fibonacci", mainFibonacci)
  , ("pascal", mainPascal)
  , ("mingle", mainMingle)
  , ("swap", mainSwap)
  , ("convexHull", mainConvexHull)
  , ("reduction", mainReduction)
  , ("filterGTk", mainFilterGTk)
  , ("sumOfPowers", mainSumOfPowers)
  , ("sierpinski", mainSierpinski)
  , ("superDigit", mainSuperDigit)
  , ("tree", mainTree)
  ]

