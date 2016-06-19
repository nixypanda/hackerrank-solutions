import qualified Data.Vector as Vec
import Data.Vector ((!)) 

---------------------------------------------------------------------------------------------------

{-|
 - Calculate the nth pentagonal number
 -}
pentagonal :: (Integral a) => a -> a
pentagonal n = n * (3 * n - 1) `div` 2
---------------------------------------------------------------------------------------------------

{-|
 - Infinite list of fibonacci number modulo 10^8 + 7
 - i.e. 0, 1, 1, 2, 3, 5, 8 ....
 -}
fibs :: [(Int, Int)]
fibs = zip [0..] fs
  where
    fs     = 0 : 1 : zipWith (\x y -> modl (modl x + modl y)) fs (tail fs)
    modl x = mod x (10^8 + 7)

{-|
 - Takes in a list of indicies in increasing order and for every index gets the value of the
 - the fibonacci number (mod 10^8 + 7).
 -}
rFibs :: (Eq a) => [a] -> [(a, t)] -> [t]
rFibs [] _               = []
rFibs (x:xs) ((i, f):fs) = if x == i then f : rFibs xs fs else rFibs (x:xs) fs
---------------------------------------------------------------------------------------------------

{-|
 - Creates a pascal triangle with 1001 rows
 - i.e.
 - 1
 - 1 1
 - 1 2 1
 - 1 3 3 1
 - .....
 -}
pascal :: Vec.Vector (Vec.Vector Int)
pascal = Vec.fromList $ take 1001 $ map Vec.fromList nCr
  where
    modl x          = mod x (10^8+7)
    nCr             = [1, 0] : [1, 1, 0] : map (\x -> 1:nVec x) (tail nCr)
    nVec [0]        = [0]
    nVec (y1:y2:ys) = modl (modl y1 + modl y2) : nVec (y2:ys)

{-|
 - Takes a list of (row, col) indices and returns a list with the pascal triangle
 - values at that location.
 -}
repeatedPascalValues :: [(Int, Int)] -> [Int]
repeatedPascalValues = map (\(y, x) -> (pascal ! y) ! x)
---------------------------------------------------------------------------------------------------

{-|
 - Generates an infinite list of list of number of binary search trees. Weird
 - [1], [1, 1], [2, 1, 1], [5, 2, 1, 1], [14, 5, 2, 1, 1] ......
 - i.e. the first entry of the nth row will give the binary search trees that can exist for that
 - n.
 -}
binarySearchTrees = iterate trees [1, 1]
  where
    trees xs    = foldr modadd 0 (zipWith modmult xs (reverse xs)) : xs
    modadd x y  = modl (modl x + modl y)
    modmult x y = modl (modl x * modl y)
    modl x      = mod x (10^8 + 7)

