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

{-|
 - A binary tree is a tree where each node has at most two children. It is characterized by any of
 - the following properties:
 - 
 - * It can be an empty tree, where root = null.
 - * It can contain a root node which contain some value and two subtree, left subtree and right
 - subtree, which are also binary tree.
 - A binary tree is a binary search tree (BST) if all the non-empty nodes follows both two
 - properties:
 - 
 - * Each node's left subtree contains only values less than it, and
 - * Each node's right subtree contains only values greater than it.
 - 
 - Preorder traversal is a tree traversal method where the current node is visited first, then
 - the left subtree and then the right subtree. More specifically, let's represent the preorder
 - traversal of a tree by a list. Then this list is constructed in following way:
 - 
 - * If the tree is empty, then this list be a null list.
 - * For non-empty tree, let's represent the preorder of left subtree as L and of right subtree
 - as R. Then the preorder of tree is obtained by appending L to current node, and then appending
 - R to it.
 - 
 - 1         2          3
 -  \       / \        / \
 -   3     1   3      2   5
 -  /                /   / \
 - 2                1   4   6
 - (a)       (b)        (c)
 - For the above trees, preorder will be
 - 
 - (a) 1 3 2
 - (b) 2 1 3
 - (c) 3 2 1 5 4 6
 - 
 - Given a list of numbers, determine whether it can represent the preorder traversal of a binary
 - search tree(BST).
 -}
mainValidBST :: IO ()
mainValidBST = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \_ -> do
    ns <- fmap (map (read :: String -> Int) . words) (getLine >> getLine)
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


{-|
 - We call an integer p > 1 a prime number (or simply a prime) if its only positive divisors are 1
 - and p. 
 -
 - Fundamental theorem of arithmetic states: Every positive integer n can be uniquely expressed
 - as a product of power of primes, i.e.,
 - N = p1 ^ n1 * p2 ^ n2 ...
 - where, 
 - pi is the ith prime, i.e., p1 = 2, p2 = 3 ....
 - forall i . ni >= 0
 -
 -
 - Greatest common divisor of two positive integers 
 - For two positive integers, A and B, whose prime factorization is represented as 
 - A = p1 ^ a1 * p2 ^ a2 ....
 - B = p1 ^ b1 * p2 ^ b2 ....
 -
 - We calculate the greatest common divisor, gcd(A, B), as
 - gcd(A, B) = p1 ^ (min a1 b1) * p2 (min a2 b2) ...
 -
 -
 - Greater common divisor of a list of numbers
 - Greatest common factor of a list of positive integers, [ l1, l2, ..., ln ], is represented as 
 - gcd(lst) = gcd l1 (gcd l2 ...... gcd l{n-1} ln))))
 -
 - Finite representation of prime factorization 
 - Since primes are infinite, it is not possible to store factors in the form provided above.
 - To that end, we will only consider those prime factors (pi) whose power is greater than
 - zero (ni > 0). That is:
 - N = pi1 ^ ni1 * pi2 ^ ni2 ....
 - , where
 - pij < pi{j + 1}
 - 0 < nij, j E [1,2..n]; for rest ni = 0 
 - And we will represent them as following:
 - N = pi1 ni1 pi2 ni2 pi3 ni3 ....
 - 
 - For example:
 - 49 = 7^2 = 7 2
 - 28 = 2^2 * 7^1 = 2 2 7 1
 -
 - Challenge:
 - You are given the elements of list, lst, in the representation provided above. Find its
 - greatest common divisor, i.e., gcd(lst).
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

