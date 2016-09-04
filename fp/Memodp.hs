{-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -Werror #-}

module MemoDP
  ( memodp
  , modadd
  , modmult
  , modl
  )
  where


import Control.Monad.ST (ST, runST)
import Data.Vector ((!)) 
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV


-- HELPERS ----------------------------------------------------------------------------------------

{-| Gets the mod of a number wrt 100000007 -}
-- to avoid warnings adding types
modl :: Integer -> Integer
modl x =
  x `mod` ((10^(8 :: Integer) + 7) :: Integer)


{-| Modular addition wrt 100000007 -}
modadd :: Integer -> Integer -> Integer
modadd x y =
  modl (modl x + modl y)


{-| Modular multiplication wrt 100000007 -}
modmult ::  Integer -> Integer -> Integer
modmult x y =
  modl (modl x * modl y)


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


binarySearchTrees' :: V.Vector Integer
binarySearchTrees' =
  V.fromList $ reverse (binarySearchTrees !! 1000)

{-|
 - A binary tree is a tree which is characterized by any of the following properties:
 - 
 - It can be empty (null).
 - It can contain a root node which contain some value and two subtree, left subtree and right
 - subtree, which are also binary tree.
 - A binary tree is a binary search tree (BST) if all the non-empty nodes follows both two
 - properties:
 - 
 - If node has a left subtree, then all the values in its left subtree are smaller than the value
 - of the current node.
 - If node has a right subtree, then all the value in its right subtree are greater than the value
 - of the current node.
 - You are given N nodes, each having unique value ranging from [1, N], how many different binary
 - search tree can be created using all of them.
 -}
mainBinarySearchTrees :: IO ()
mainBinarySearchTrees =
  getContents >>= putStr . unlines . map (show . (binarySearchTrees' !) . read) . tail . lines


-- PENTAGONAL NUMBERS -----------------------------------------------------------------------------

{-|
 - Calculate the nth pentagonal number
 -}
pentagonal :: Integer -> Integer
pentagonal n =
  n * (3 * n - 1) `div` 2

{-|
 - Pentagonal numbers are the number of dots that can be shown in a pentagonal pattern of dots.
 - Let's represent the nth pentagonal number by P(n). The following figure depicts pentagonal
 - patterns for n ∈ {1, 2, 3, 4, 5}.
 -
 - Your task is to find the value of P(n) for a given n.
 -}
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


{-|
 - The original problem statment where Fibonacci series appears for the first time in modern
 - period is a very interesting one. It was a book by Leonard of Pisa, also known as Fibonacci,
 - named Liber Abaci (1202) which brought such a intersting series to the popularity.
 - 
 - Fibonacci considers the growth of an idealized (biologically unrealistic) rabbit population,
 - assuming that: a newly born pair of rabbits, one male, one female, are put in a field; rabbits
 - are able to mate at the age of one month so that at the end of its second month a female can
 - produce another pair of rabbits; rabbits never die and a mating pair always produces one new
 - pair (one male, one female) every month from the second month on. The puzzle that Fibonacci
 - posed was: how many pairs will there be in one year?
 - 
 - At the end of the first month, they mate, but there is still only 1 pair.
 - At the end of the second month the female produces a new pair, so now there are 2 pairs of
 - rabbits in the field.
 - At the end of the third month, the original female produces a second pair, making 3 pairs in
 - all in the field.
 - At the end of the fourth month, the original female has produced yet another new pair, the
 - female born two months ago produces her first pair also, making 5 pairs.
 - At the end of the nth month, the number of pairs of rabbits is equal to the number of new pairs
 - (which is the number of pairs in month n − 2) plus the number of pairs alive last month (n − 1).
 - This is the nth Fibonacci number.
 - 
 - This series can be broken down as the following series: 
 - Fib0 = 0 
 - Fib1 = 1 
 - Fibn = Fibn-1 + Fibn-2 , n > 1
 - 
 - First few elements of Fibonacci series are:
 - 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377...
 - 
 - You are given a list of non-negative integers. For each integer, n, in the list print nth
 - fibonacci number modulo 108+7.
 -}
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
pascal' :: V.Vector (V.Vector Integer)
pascal' =
  V.fromList $ take 1001 $ map V.fromList pascal


{-|
 - Arsenal have been on a losing streak for years now. Their critic have gone to the extent of
 - saying that they can't even defeat a team of lemurs. So Arsenal decided to show them how wrong
 - they are. But they have done a grave mistake. You are in charge of lemur team and are confident
 - of the abilities of a lemur. Given N lemurs, you have to select K out of them who will be
 - facing off Arsenal in a soccer match.
 - 
 - You have to count total number of different teams you can form by selecting K out of N lemurs.
 - A team is different from other team if there's exist atleast one lemur who is in one team but
 - not in other. As this number can be large print answer modulo (10^8+7).
 - 
 - Let's say count(N, K) represent how many different team can be formed by selecting K out of N
 - lemurs. Then any of the following condition may occur:
 - 
 - K = 0, that is you have to select no one. There only one way of doing that, ie, selecting no one.
 - K = N, you have to select all of them. Here, also, is only way of doing that, ie, selecting all.
 - 0 < K < N, let's number N lemurs from [1, N]. Then there are two option
 -  * Select first lemur, and then select (K-1) lemurs from remaining (N-1) lemurs.
 -  * Don't select first lemur, so you have to select K lemurs from remaining (N-1) lemurs.
 -
 - Mathematically, we can represent this situation as following
 - 
 -               1                                   , K = 0
 - count(N, K) = 1                                   , K = N
 -               count(N-1, K-1) + count(N-1, K),    , 0 < K < N
 -
 - Note: Haskell users can use Data.Vector library.
 -}
mainPascal :: IO ()
mainPascal =
  getContents >>=
    putStr . unlines . map (show . (\[y, x] -> (pascal' ! y) ! x) . map read . words) . tail . lines


-- EXPRESSIONS ------------------------------------------------------------------------------------

{-|
 - Treating the list of numbers as a stack and trying out every sign (+), (-) and (*) then
 - push the result back on the stack and recursing.
 -
 - Haskell's laziness is preety much saving my ass here.
 -}
div101 :: String -> [Integer] -> String
div101 _ [] = error "where did the total go"
div101 s [t] = if t `mod` 101 == 0 then s else ""
div101 s (t:x:xs) =
  let
    plus  = div101 (s ++ "+" ++ show x) ((t + x):xs)
    minus = div101 (s ++ "-" ++ show x) ((t - x):xs)
    mult  = div101 (s ++ "*" ++ show x) ((t * x):xs)
  in
    if mult /= "" then mult else if plus /= "" then plus else minus


divBy101 :: [Integer] -> String
divBy101 [] = ""
divBy101 (x:xs) = div101 (show x) (x:xs)


{-|
 - 5 year old Shinchan had just started learning Mathematics. Meanwhile, one of his studious
 - classmate, Kazama, had already written a basic calculator which supports only 3 operations
 -  on integral numbers: multiplication , addition , and subtraction . Since he had just learnt
 -  about these operations, he didn't have knowledge of precedence of operators, and in his
 -  calculator all operators had same precedence and left associativity. 

 - As always Shinchan started to irritate him with his silly question. He gave Kazama a list of
 - integers and asked him to insert one of the above operators between each pair of consecutive
 - integer such that the result obtained after feeding the resulting expression in Kazama's
 - calculator is divisible by . At core Shinchan is a good guy, so he gave only that list of
 - integers for which the answer always exists. 
 - 
 - Can you help Kazama in creating the required expression? If multiple solutions exists, printx
 - any one of them. 
 -}
mainExpressions :: IO ()
mainExpressions =
  getLine >> getLine >>= putStrLn . divBy101 . map read . words


-- DICE PATHS -------------------------------------------------------------------------------------

{-| The direction in which the die can e rolled. -}
data Direction
  = RollRight 
  | RollDown
  deriving (Show)


{-| The maxscore with the given number facing upside. -}
data Score = Score
  { one :: Int
  , two :: Int
  , three :: Int
  , four :: Int
  , five :: Int
  , six :: Int
  } deriving (Show)


{-| The 6-tuple representing a Die -}
type Die =
  (Int, Int, Int, Int, Int, Int)

type Grid s =
  MV.MVector s Score

{-|
 - Creates a new 2-d mutable vector that will hold scores (max-possible) at every combination of
 - valid row column pairs.
 -}
newGrid :: Int -> Int -> ST s (Grid s)
newGrid r c =
  MV.replicate (r * c) noScore


-- fillRightsInitial :: ST s (Grid s) -> Int -> ST s (Grid s)
-- fillRightsInitial g l = do
--   g' <- g
--   forM_ (zip [1..l] (iterate (rotate RollRight) initialConfig)) $ \(i, dc) -> do
--     MV.write g' i (noScore {top = (fst dc)})
--   return g'


dp = do
  g <- newGrid 60 60
  MV.write g 0 initialScore
  return g


{-| The score at (1, 1) -}
initialScore :: Score
initialScore =
  Score 1 0 0 0 0 0

noScore :: Score
noScore =
  Score 0 0 0 0 0 0

initialConfig :: Die
initialConfig =
  (1, 6, 3, 4, 2, 5)


{-| The function which will take in a die and a direction a give a die rotated in that direction -}
rotate :: Direction -> Die -> Die
rotate RollDown (top, bottom, left, right, front, back)  = (front, back, left, right, bottom, top)
rotate RollRight (top, bottom, left, right, front, back) = (right, left, top, bottom, front, back)


mainDice :: IO ()
mainDice = 
  getLine >> getContents >>=
    print . map ((\[x, y] -> (x, y)) . map (read :: String -> Int) . words) . lines

-- MAIN -------------------------------------------------------------------------------------------

memodp :: [(String, IO ())]
memodp =
  [ ("pentagonal", mainPentagonal)
  , ("modularFibonacci", mainFibonacci)
  , ("modularPascal", mainPascal)
  , ("modularBinarySerachTrees", mainBinarySearchTrees)
  , ("expressions", mainExpressions)
  ]

