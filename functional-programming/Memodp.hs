{-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -Werror #-}

module MemoDP
  ( memodp
  , modadd
  , modmult
  , modl
  )
  where


import qualified Data.Vector as Vec
import Data.Vector ((!)) 


-- HELPERS ----------------------------------------------------------------------------------------

{-| Gets the mod of a number wrt 100000007 -}
modl :: Integer -> Integer
modl x =
  x `mod` (10^8 + 7)


{-| Modular addition wrt 100000007 -}
modadd :: Integer -> Integer -> Integer
modadd x y =
  modl (modl x + modl y)


{-| Modular multiplication wrt 100000007 -}
modmult ::  Integer -> Integer -> Integer
modmult x y =
  modl (modl x * modl y)


-- PENTAGONAL NUMBERS -----------------------------------------------------------------------------

{-|
 - Calculate the nth pentagonal number
 -}
pentagonal :: Integer -> Integer
pentagonal n =
  n * (3 * n - 1) `div` 2

mainPentagonal :: IO ()
mainPentagonal =
  getContents >>= putStr . unlines . map (show . pentagonal . read) . tail . lines


-- FIBONACCI --------------------------------------------------------------------------------------

{-|
 - Infinite list of fibonacci number modulo 10^8 + 7
 - i.e. 0, 1, 1, 2, 3, 5, 8 ....
 -}
modFibonacci :: [Integer]
modFibonacci =
  0 : 1 : zipWith modadd modFibonacci (tail modFibonacci)

{-|
 - Takes in a list of indicies in increasing order and for every index gets the value of the
 - the fibonacci number (mod 10^8 + 7).
 -}
rFibs :: Integer -> [Integer] -> [Integer] -> [Integer]
rFibs _ _ [] = []
rFibs _ [] _ = error "Impossible"
rFibs i (f:fs) (x:xs) =
  if x == i then f : rFibs (i + 1) fs xs else rFibs (i + 1) fs (x:xs)

mainFibonacci :: IO ()
mainFibonacci =
  getContents >>= putStr . unlines . map show . rFibs 0 modFibonacci . map read . tail . lines


-- DIFFERENT WAYS ---------------------------------------------------------------------------------

{-|
 - Creates the complete infinite pascal triangle but which is mudular wrt 100000007
 - i.e.
 - 1
 - 1 1
 - 1 2 1
 - 1 3 3 1
 - .....
 -}
pascal :: [[Integer]]
pascal =
  [1, 0] : [1, 1, 0] : map (\x -> 1 : nVec x) (tail pascal)
    where
      nVec :: [Integer] -> [Integer]
      nVec [] = error "This cannot happen"
      nVec [0] = [0]
      nVec [_] = error "This cannot happen"
      nVec (y1:y2:ys) = modadd y1 y2 : nVec (y2:ys)


-- | Pascal triangle with 1001 rows for easier lookup
pascal' :: Vec.Vector (Vec.Vector Integer)
pascal' =
  Vec.fromList $ take 1001 $ map Vec.fromList pascal


{-|
 - Takes a list of (row, col) indices and returns a list with the pascal triangle
 - values at that location.
 -}
mainPascal :: IO ()
mainPascal =
  getContents >>=
    putStr . unlines . map (show . (\[y, x] -> (pascal' ! y) ! x) . map read . words) . tail . lines


-- BINARY SEARCH TREES ----------------------------------------------------------------------------

{-|
 - Generates an infinite list of list of number of binary search trees. Weird
 - [1], [1, 1], [2, 1, 1], [5, 2, 1, 1], [14, 5, 2, 1, 1] ......
 - i.e. the first entry of the nth row will give the binary search trees that can exist for that
 - n.
 -}
binarySearchTrees :: [[Integer]]
binarySearchTrees = iterate trees [1, 1]
  where
    trees xs = foldr modadd 0 (zipWith modmult xs (reverse xs)) : xs


binarySearchTrees' :: Vec.Vector Integer
binarySearchTrees' =
  Vec.fromList $ reverse (binarySearchTrees !! 1000)


mainBinarySearchTrees :: IO ()
mainBinarySearchTrees =
  getContents >>= putStr . unlines . map (show . (binarySearchTrees' !) . read) . tail . lines

---------------------------------------------------------------------------------------------------

memodp :: [(String, IO ())]
memodp =
  [ ("pentagonal", mainPentagonal)
  , ("modularFibonacci", mainFibonacci)
  , ("modularPascal", mainPascal)
  , ("modularBinarySerachTrees", mainBinarySearchTrees)
  ]

