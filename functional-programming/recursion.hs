-- need intersperse
import Data.List
import Data.Function (on)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Char as Char

---------------------------------------------------------------------------------------------------

{-| Compoute the greatest common divisor of two numbers -}
gcd' :: (Ord a, Num a) => a -> a -> a
gcd' m n
    | m == n = n
    | m > n = gcd' (m - n) n
    | m < n = gcd' (n - m) m
---------------------------------------------------------------------------------------------------

{-|
 - Infinite list of fibonacci numbers. To get nth just do fibs !! n
 -}
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
---------------------------------------------------------------------------------------------------

{-|
 - Returns an infinite list of pascal triangle rows.
 -}
pascal :: [[Integer]]
pascal = map init nCr
  where
    nVec [0] = [0]
    nVec (y1 : y2 : ys) = (y1 + y2) : nVec (y2 : ys)
    nCr = [1, 0] : [1, 1, 0] : map (\x -> 1 : nVec x) (tail nCr)
---------------------------------------------------------------------------------------------------

{-|
 - Generate the ever popular sierpinski triangle using '1' and '_' on 'rows' number of rows (should
 - be 2^k - 1 where k <- Natural numbers and 'n' recursive depth. more the n more preety the
 - pattern and will require more number of lines.
 -}
spnsk n rows
    -- 1 -> generate a triangle of ones with given number of rows
    | n == 1 = triangle ( 2 * rows + 1)
    -- construct bottom half by putting two of them (smaller) side-by-side
    -- construct top half by padding it (smaller) with '_'
    -- and just put them together
    | otherwise = zipWith (curry ubar) smaller smaller ++ map (ubars (m + 1)) smaller
    where m           = rows `div` 2
          ubar (x, y) = x ++ ['_'] ++ y
          -- n -> generate pattern for n - 1: smaller
          smaller     = spnsk (n - 1) m
          ubars n lst = replicate n '_' ++ lst ++ replicate n '_'
          triangle 1  = ["1"]
          -- n ones prepended to others which are prepended and appended with 1 '_'
          triangle n  = replicate n '1' : map (ubars 1) (triangle (n - 2))

-- sierpinski n rows = (putStr . unlines . reverse) (spnsk (n + 1) rows)
---------------------------------------------------------------------------------------------------

{-| mingle together multiple strings -}
mingle :: [a] -> [a] -> [a]
mingle [] []         = []
mingle (x:xs) (y:ys) = x : y : mingle xs ys
---------------------------------------------------------------------------------------------------

{-| swap numbers at even odd -}
swap :: [a] -> [a]
swap []         = []
swap (x1:x2:xs) = x2 : x1 : swap xs
---------------------------------------------------------------------------------------------------

-- P7: Recursive tree (needs cleanup
-- [String]
why r ch = slant (r - 1) ++ straight r
    where repl_ n     = replicate n '_'
          ubars n lst = repl_ n ++ lst ++ repl_ n
          straight n  = replicate n (repl_ n ++ [ch] ++ repl_ n)
          slant 0     = [[ch, '_', ch]]
          slant n     = ([ch] ++ repl_ (2 * n + 1) ++ [ch]):map (ubars 1) (slant (n - 1))

-- ubar :: (String, String) -> String

-- [String]
rTree 1 n co
    | n >= co = why (16 `div` (2^n)) '_'
    | otherwise = why (16 `div` (2^n)) '1'

rTree n r co = map (ubar 1) (zip smaller smaller) ++ map (\x -> ubars ++ x ++ ubars) (rTree 1 r co)
    where smaller       = rTree (n - 1) (r + 1) co
          ubars         = replicate (2^(n - 1) - 1) '_'
          ubar n (x, y) = x ++ replicate n '_' ++ y

tree n = concat (replicate 100 "_") : map (\x -> replicate 18 '_' ++ x ++ replicate 19 '_') (rTree 5 0 n)
---------------------------------------------------------------------------------------------------

-- BUG: NOT WORKING
-- perimeter of the ploygon whose verticies are given in order
perimeter lst = foldl (+) 0.0 (zipWith distance lst (tail (cycle lst)))
    -- get distance between two points on x-y plane
    where distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

grahamScan pts
    | length pts >= 3 = scan [p0] rests
    | otherwise       = pts
    where p0                             = minimumBy (compare `on` snd) pts -- point with the minimum y
          compkey (x0, y0) (x, y)        = (atan2 (y - y0) (x - x0), abs (x - x0))
          rests                          = tail (sortBy (compare `on` compkey p0) pts)
          ccw (ax, ay) (bx, by) (cx, cy) = compare ((bx - ax) * (cy - ay)) ((cx - ax) * (by - ay))
          scan xs [z]                    = z : xs
          scan [p0] (p1:ps)              | ccw (last ps) p0 p1 == EQ = [last ps, p0]
          scan (x : xs) (y : z : rsts)   = case ccw x y z of
                                              LT -> scan xs (x:z:rsts)
                                              EQ -> scan (x:xs) (z:rsts) -- skip collinear points
                                              GT -> scan (y:x:xs) (z:rsts)

convexHull = perimeter . grahamScan
---------------------------------------------------------------------------------------------------

{-|
 - Filters out all the repeated elements while preserving the order in which the elements first
 - appear unique elements of a list
 -}
reduction :: (Ord a) => Set.Set a -> [a] -> [a]
reduction seen []     = []
reduction seen (x:xs) =
  if x `Set.member` seen
  then reduction seen xs
  else x : reduction (x `Set.insert` seen) xs
---------------------------------------------------------------------------------------------------

{-|
 - Only elements whose count is greater than k in the given list whilest preserving the first
 - occurance order.
 -}
filterListGTk :: (Ord t1, Ord t, Num t1, Num t) => ([t1], t) -> [t1]
filterListGTk (xs, k) =
    if null filterd
    then [ -1 ]
    else reduction Set.empty filterd
    where mapF    = Map.fromListWith (+) [(x, 1) | x <- xs]
          filterd = filter (\x -> maybe False (>= k) (Map.lookup x mapF)) xs
---------------------------------------------------------------------------------------------------

{-|
 - Returns the number of unique ways we can get n by adding kth powers of natural numbers.
 -}
sumOfPows :: (Ord a, Num a, Enum a, Integral b) => a -> b -> Int
sumOfPows n k = length $ filter (\x -> sum x == n) $ pset n 0 $ takeWhile (<= n) $ map (^k) [1..]
  where
    -- returns the pset of xs | sum xs < n (reduces the search space by quite a lot)
    pset n s [] = [[]]
    pset n s (x:xs)
      | s + x > n = pset n s xs
      | otherwise = pset n s xs ++ map (x:) (pset n (s + x) xs)
---------------------------------------------------------------------------------------------------

{-|
 - Returns super digit of a number 
 - We define super digit of an integer x using the following rules:
 - 
 - Iff x has only 1 digit, then its super digit is x.
 - Otherwise, the super digit of x is equal to the super digit of the digit-sum of x. Here,
 - digit-sum of a number is defined as the sum of its digits.
 -}
kSuperDigit :: String -> Int -> Int
kSuperDigit n k = superDigit . show $ k * superDigit n
  where
    superDigit [a] = Char.digitToInt a
    superDigit xs  = superDigit . show . sum $ map Char.digitToInt xs
---------------------------------------------------------------------------------------------------

