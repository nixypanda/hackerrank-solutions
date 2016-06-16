-- need intersperse
import Data.List
import Data.Function (on)
import qualified Data.Map as Map
import qualified Data.Set as Set

---------------------------------------------------------------------------------------------------
-- P1: compoute the greatest common divisor
gcd' m n
    | m == n = n
    | m > n = gcd' (m - n) n
    | m < n = gcd' (n - m) m

---------------------------------------------------------------------------------------------------

-- P2: calculate the nth fibonacci number
fib = fibonacci 0 1 1
    where fibonacci a b n lim = if n == lim then a else fibonacci b (a + b) (n + 1) lim

---------------------------------------------------------------------------------------------------

-- P3: print the pascal triangle onto the screen
pascal n = (putStr . unlines) [ unwords (map show (pascal_row r)) | r <- [0..(n - 1)] ]
    where fact 0       = 1
          fact n       = n * fact (n - 1)
          -- pascal row offset by one
          pascal_row n = [ fact n `div` (fact r * fact (n - r)) | r <- [0..n] ]

---------------------------------------------------------------------------------------------------

-- n -> generate pattern for n - 1: smaller
-- construct bottom half by putting two of them (smaller) side-by-side
-- construct top half by padding it (smaller) with '_'
-- and just put them together
spnsk n rows
    -- 1 -> generate a triangle of ones with given number of rows
    | n == 1 = triangle ( 2 * rows + 1)
    | otherwise = zipWith (curry ubar) smaller smaller ++ map (ubars (m + 1)) smaller
    where m           = rows `div` 2
          ubar (x, y) = x ++ ['_'] ++ y
          smaller     = spnsk (n - 1) m
          ubars n lst = replicate n '_' ++ lst ++ replicate n '_'
          triangle 1  = ["1"]
          -- n ones prepended to others which are prepended and appended with 1 '_'
          triangle n  = replicate n '1' : map (ubars 1) (triangle (n - 2))

sierpinski n rows = (putStr . unlines . reverse) (spnsk (n + 1) rows)

---------------------------------------------------------------------------------------------------

-- P5: mingle together multiple strings
mingle [] []         = []
mingle (x:xs) (y:ys) = x : y : mingle xs ys

---------------------------------------------------------------------------------------------------

-- P6: swap even odd numbers 
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
          scan [p0] (p1:ps)              | ccw (last ps) p0 p1 == EQ = [(last ps), p0]
          scan (x : xs) (y : z : rsts)   = case ccw x y z of
                                              LT -> scan xs (x:z:rsts)
                                              EQ -> scan (x:xs) (z:rsts) -- skip collinear points
                                              GT -> scan (y:x:xs) (z:rsts)

convexHull = perimeter . grahamScan
------------------------------------------------------------------------------------------------------

-- unique elements of a list
reduction seen []     = []
reduction seen (x:xs) =
  if x `Set.member` seen
  then (reduction seen xs)
  else x:(reduction (x `Set.insert` seen) xs)

-------------------------------------------------------------------------------------------------------

-- only elements whose count is greater than k
fk (xs, k) =
    if null filterd
    then [ -1 ]
    else reduction Set.empty filterd
    where mapF    = Map.fromListWith (+) [(x, 1) | x <- xs]
          filterd = filter (\x -> maybe False (>= k) (Map.lookup x mapF)) xs

-------------------------------------------------------------------------------------------------------

-- returns the powerset of xs | sum xs < n (reduces the search space by quite a lot)
powerset n s [] = [[]]
powerset n s (x:xs)
  | s + x > n = powerset n s xs
  | otherwise = (powerset n s xs) ++ map (x:) (powerset n (s + x) xs)

sumOfPowers n k = length (filter (\x -> (sum x) == n) (powerset n 0 (takeWhile (<= n) (map (^k) [1..]))))
