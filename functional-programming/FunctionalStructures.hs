{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}


module FunctionalStructures
  ( functionalStructures
  ) where


import Control.Monad (replicateM, forM_)


-- GENERAL HELPERS --------------------------------------------------------------------------------

{-|
 - Creates chunks of size k for the given list
 - e.g. chunksOf 2 [1..5] = [[1, 2], [3, 4], [5]]
 -}
-- recursivly split at the specified chunk size.
-- split ar location k call left one ys and right one zs, then recursivly do the same to zs.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = ys : chunksOf k zs
  where (ys, zs) = splitAt k xs


-- Binary Search Tree --

{-|
 - A tree is either an empty tree or it's an element that contains some value and two trees.
-}
data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving (Show, Read, Eq)


{-|
 - Creates a tree with a single node
 -}
singleton :: a -> Tree a
singleton a =
  Node a Empty Empty


{-|
 - Insert an element into the binary search tree
 -}
treeInsert :: (Ord a) => Tree a -> a -> Tree a
treeInsert Empty x = singleton x
treeInsert (Node y left right) x
  | x > y     = Node y left (treeInsert right x)
  | x < y     = Node y (treeInsert left x) right
  | otherwise = Node x left right


{-|
 - Create a tree from the provided list
 -}
fromList :: Ord a => [a] -> Tree a
fromList =
  foldl treeInsert Empty


{-|
 - Preorder traversal of the given tree
 -}
preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node y left right) = 
  y : (preorder left ++ preorder right)



-- Valid BST --------------------------------------------------------------------------------------

{-|
 - Given the preorder traversal check if the given binary tree is valid or not
 -}
isValid :: Ord a => [a] -> Bool
isValid ns =
  (preorder . fromList) ns == ns



mainValidBST :: IO ()
mainValidBST = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \_ -> do
    _ <- readLn :: IO Int
    ns <- fmap (map (read :: String -> Int) . words) getLine
    putStrLn $ if isValid ns then "YES" else "NO"


-- LISTS AND GCD ----------------------------------------------------------------------------------

{-
 - Given two lists where each element is of the form (prime : p_i, power of prime : e_i).
 - such that they both make up the number p_1 ^ e_1 * p_2 ^ e_2 .... each. We find the gcd
 - of the two numbers:
 - A = p_1 ^ a_1 * p_2 ^ a_2 ....
 - B = p_1 ^ b_1 * p_2 ^ b_2 ....
 - gcd(A, B) = p_1 ^ (min a_1 b_1) * p_2 (min a_2 b_2) ...
 -}
gcd' :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
gcd' [] [] = []
gcd' [] _ = []
gcd' _ [] = []
gcd' xs'@((xb, xe):xs) ys'@((yb, ye):ys)
  | xb < yb = gcd' xs ys'
  | xb > yb = gcd' xs' ys
  | otherwise = (xb, min xe ye) : gcd' xs ys


{-
 - Given a list of positive numbers. Calculates the greates common divior.
 - lst = [ l_1, l_2, ..., l_n ]
 - gcd(lst) = gcd l_1 (gcd l_2 ...... gcd l_{n-1} l_n))))
 -}
gcd'' :: [[(Int, Int)]] -> [(Int, Int)]
gcd'' = foldr1 gcd'


{-
 - You are given the elements of list, in the representation provided above.
 - Find its greatest common divisor.
 -}
mainListsAndGCD :: IO ()
mainListsAndGCD = do
  t <- readLn :: IO Int
  a <- replicateM t (fmap (map (\[x, y] -> (x, y)) . chunksOf 2 . map read . words) getLine)
  (putStr . unwords . map show . concatMap (\(x, y) -> [x, y]) . gcd'') a


-- MAIN -------------------------------------------------------------------------------------------

functionalStructures :: [(String, IO ())]
functionalStructures =
  [ ("listGCD", mainListsAndGCD)
  , ("validBST", mainValidBST)
  ]

