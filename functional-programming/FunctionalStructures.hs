{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}


module FunctionalStructures
  ( UnionFind
  , union
  , find
  , newUF
  , connected
  , functionalStructures
  ) where


import Control.Monad
  ( foldM
  , forM_
  , liftM2
  , replicateM
  , when
  )
import Control.Monad.ST (ST, runST)
import Data.List (sort)

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV


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


{-|
 - Inorder traversal of the given tree
 -}
inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node y left right) =
  inorder left ++ [y] ++ inorder right


{-|
 - Preety prints the given tree onto the screen
 - *Main> let tree = fromList [1..10]
 - *Main> putStr $ showTree tree
 -  7*
 -  +- 5
 -  |  +- 3*
 -  |  |  +- 2
 -  |  |  |  +- 1*
 -  |  |  `- 4
 -  |  `- 6
 -  `- 9
 -     +- 8
 -     `- 10
 -
 -}
-- showTree :: Show a => Tree a -> String
-- showTree Empty = "Empty root."
-- showTree (Node node left right) =
--   unlines (ppHelper (Node node left right))
--
--     where
--       pad :: String -> String -> [String] -> [String]
--       pad first rest =
--         zipWith (++) (first : repeat rest)
--
--       ppSubtree :: Show a => Tree a -> Tree a -> [String]
--       ppSubtree left right =
--         pad "+- " "|  " (ppHelper left) ++ pad "`- " "   " (ppHelper right)
--
--       ppHelper :: Show a => Tree a -> [String]
--       ppHelper Empty = []
--       ppHelper (Node node left right) =
--         show node : ppSubtree left right


-- SWAP NODES -------------------------------------------------------------------------------------

{-|
 - Given a tree and a integer, K, swaps the subtrees of all the nodes who are at depth h,
 - where h ∈ [K, 2K, 3K,...].
 -}
swapAt :: Int -> Int -> Tree a -> Tree a
swapAt _ _ Empty = Empty
swapAt h k (Node a left right)
  | h `mod` k == 0 = Node a (swapAt (h + 1) k right) (swapAt(h + 1) k left)
  | otherwise = Node a (swapAt (h + 1) k left) (swapAt (h + 1) k right)

{-|
 - Construct the tree from a given list of tuples where ith tuple represents the children of ith
 - node.
 -}
fromArray :: Int -> [(Int, Int)] -> Tree Int
fromArray i arr
  | i == -1 = Empty
  | otherwise =
    Node i (fromArray x arr) (fromArray y arr) where (x, y) = arr !! (i - 1)

{-|
 - Given a function list and an initial tree apply the first function then give the result to the
 - second then result from that to third and so on.
 -}
repeatedFs :: [Tree a -> Tree a] -> Tree a -> [Tree a]
repeatedFs [] _ = []
repeatedFs (f:fs) a' = f a' : repeatedFs fs (f a')

{-|
 - Given a list of tuples and a list of int. Constructs a tree from the list of tuples then
 - swaps the nodes at k * nth (n E natural numbers) for every k in the other list, and gives
 - inorder traversals of all the trees that the initial tree became after each step.
 -}
swapNodes :: [(Int, Int)] -> [Int] -> [[Int]]
swapNodes a ks =
  map inorder $ repeatedFs (map (swapAt 1) ks) (fromArray 1 a)

{-|
 - A binary tree is a tree which is characterized by any one of the following properties:
 -
 - It can be an empty (null).
 - It contains a root node and two subtrees, left subtree and right subtree. These subtrees are
 - also binary tree.
 - Inorder traversal is performed as
 -
 - Traverse the left subtree.
 - Visit root (print it).
 - Traverse the right subtree.
 - We define depth of a node as follow:
 -
 - Root node is at depth 1.
 - If the depth of parent node is d, then the depth of current node wll be d+1.
 - Swapping: Swapping subtrees of a node means that if initially node has left subtree L and
 - right subtree R, then after swapping left subtree will be R and right subtree L.
 -
 - Eg. In the following tree, we swap children of node 1.
 -
 -                                 Depth
 -     1               1            [1]
 -    / \             / \
 -   2   3     ->    3   2          [2]
 -    \   \           \   \
 -     4   5           5   4        [3]
 - Inorder traversal of left tree is 2 4 1 3 5 and of right tree is 3 5 1 2 4.
 -
 - Swap operation: Given a tree and a integer, K, we have to swap the subtrees of all the nodes
 - who are at depth h, where h ∈ [K, 2K, 3K,...].
 -
 - You are given a tree of N nodes where nodes are indexed from [1..N] and it is rooted at 1. You
 - have to perform T swap operations on it, and after each swap operation print the inorder
 - traversal of the current state of the tree.
 -
 - Input Format
 - First line of input contains N, number of nodes in tree. Then N lines follow. Here each of ith
 - line (1 <= i <= N) contains two integers, a b, where a is the index of left child, and b is the
 - index of right child of ith node. -1 is used to represent null node.
 - Next line contain an integer, T. Then again T lines follows. Each of these line contains an
 - integer K.
 -
 - Output Format
 - For each K, perform swap operation as mentioned above and print the inorder traversal of the
 - current state of tree.
 -
 - Constraints
 - 1 <= N <= 1024
 - 1 <= T <= 100
 - 1 <= K <= N
 - Either a = -1 or 2 <= a <= N
 - Either b = -1 or 2 <= b <= N
 - Index of (non-null) child will always be greater than that of parent.
 -}
mainSwapNodes :: IO ()
mainSwapNodes = do
  n <- readLn :: IO Int
  a <- replicateM n (fmap ((\[x,y] -> (x, y)) . map read . words) getLine)
  ks <- fmap (map read . lines) (getLine >> getContents)
  putStr . unlines . map (unwords . map show) $ swapNodes a ks


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


-- PRISON TRANSPORT -------------------------------------------------------------------------------

{-|
 - The UnionFind data type represents a union–find data type (also known as the disjoint-sets data
 - type. It supports the union and find operations, along with a connected operation for determining
 - whether two sites are in the same component.
 -
 - The union–find data type models connectivity among a set of n sites, named 0 through n - 1.
 - The is-connected-to relation must be an equivalence relation:
 -
 - * Reflexive: p is connected to p.
 - * Symmetric: If p is connected to q,then q is connected to p.
 - * Transitive: If p is connected to q and q is connected to r, then p is connected to r.
 -
 - An equivalence relation partitions the sites into equivalence classes (or components). In this
 - case, two sites are in the same component if and only if they are connected. Both sites and
 - components are identified with integers between 0 and n - 1.  Initially, there are n components,
 - with each site in its own component.  The component identifier of a component (also known as the
 - root, canonical element, leader, or set representative) is one of the sites in the component,
 - two sites have the same component identifier if and only if they are in the same component.
 -
 - * union(p, q) adds a connection between the two sites p and q. If p and q are in different
 -   components, then it replaces these two components with a new component that is the union of the
 -   two.
 - * find(p) returns the component identifier of the component containing p.
 - * connected(p, q) returns true if both p and q are in the same component, and false otherwise.
 -
 - This implementation uses weighted quick union by size (without path compression).
 -}
data UnionFind s = UnionFind
  { ids :: MV.MVector s Int
  , szs :: MV.MVector s Int
  }

{-|
 - Initialize union-find data structure with n sites. 0 through (n - 1). Each site is initially in
 - it's own component.
 -}
-- newListArray: array indexed from 0 to n - 1 containing elements of the provided list
-- newArray: create array with the provided value (in every place).
newUF :: Int -> ST s (UnionFind s)
newUF n =
  liftM2 UnionFind (V.thaw $ V.fromList [0..n - 1]) (MV.replicate n 0)

{-|
 - Returns the component identifier for the component containing the passed site.
 -}
-- get the value ar index then check if both are equal, Equality => the index is root
-- follow through the nodes until root is found
find :: UnionFind s -> Int -> ST s Int
find uf p =
  MV.read (ids uf) p >>= (\i -> if i /= p then find uf i else return i)


{-|
 - checks if the given two sites are in the same component.
 -}
-- two sites are connected if they share the root.
connected :: UnionFind s -> Int -> Int -> ST s Bool
connected uf p q =
  liftM2 (==) (find uf p) (find uf q)


{-|
 - Merges the component containing component containing the site p(or q) with the component
 - containing site q(or p). The smaller component is merged into the larger one.
 -}
union :: UnionFind s -> Int -> Int -> ST s ()
union uf p q = do
  -- get both roots
  rootP <- find uf p
  rootQ <- find uf q
  -- if they are same do nothing
  when (rootP /= rootQ) $ do
    -- otherwise get size of both the roots
    szP <- MV.read (szs uf) rootP
    szQ <- MV.read (szs uf) rootQ
    -- merge the smaller into the larger
    if szP < szQ
      then do
        MV.write (ids uf) rootP rootQ
        MV.write (szs uf) rootP 0
        MV.write (szs uf) rootQ (szP + szQ)
      else do
        MV.write (ids uf) rootQ rootP
        MV.write (szs uf) rootQ 0
        MV.write (szs uf) rootP (szP + szQ)


{-|
 - Given an integer n and a list of tuples creates a union-find data structure of size n and
 - performs union operations based on the entries for all the tuples in the list, then returns
 - a list containing the sizes of the components.
 -}
ufing :: Int -> [(Int, Int)] -> [Int]
ufing n xs = runST $ do
  uf <- newUF n
  -- subtract one as arays are zero indexed.
  forM_ (map (\(x, y) -> (x - 1, y - 1)) xs) (uncurry (union uf))
  fmap V.toList (V.freeze $ szs uf)


{-|
 - Given a number and a list of tuples figure out how many buses are required to transport the
 - n prissoners where a tuple represents that the prisoners are chained together.
 -}
prissonTrassport :: Int -> [(Int, Int)] -> Int
prissonTrassport n xs =
  let
    f :: [Int] -> [(Int, Int)] -> [Int]
    f _ [] = error "An infinite list ended!!!"
    f [] _ = []
    f (y:ys) zs'@((iz, z):zs)
      | y > z = f (y:ys) zs
      | otherwise = iz : f ys zs'
  in
    sum $ f (sort . filter (/= 0) $ ufing n xs) (zip [1..] (map (^(2 :: Int)) [1..]))

{-|
 - There are N inmates numbered between [1, N] in a prison. These inmates have superhuman strength
 - because they have drunk a special concoction made by Dr. Evil. They have to be transported by
 - some buses to a new facility. But they are bound by special chains which are made from strong
 - carbon fibres. Each inmate is either chained alone or is chained in a group along with one or
 - more inmates. A group of inmates are those who are directly or indirectly connected to each
 - other. Only one group can be transported per bus.
 -
 - There are buses which will charge fixed amount bucks for transferring inmates. Charges are
 - directly proportional to the capacity of bus. If a bus charge K bucks then it can carry upto K2
 - inmates at one time. Buses are available for all positive integral cost ranging from
 - [1, 2, 3, ...]. A bus can be used multiple times, and each time it will charge. Note that a bus
 - can also transfer less number of inmates than it's capacity.
 -
 - Find the minimal cost to transport all the inmates.
 -
 - Input
 - The first line contains N representing the number of inmates. Second line contains another
 - integer, M, number of pairs of inmates who are handcuffed together. Then follows M lines. Each
 - of these lines contains two integers, P Q, which means inmate numbered P is handcuffed to
 - inmate numbered Q.
 -
 - Output
 - For the given arrangement, print the minimal cost which can be incurred while transferring
 - inmates.
 -
 - Constraints
 - 2 ≤ N ≤ 100000
 - 1 ≤ M ≤ min(N*(N-1)/2, 100000)
 - 1 ≤ P, Q ≤ N
 - P ≠ Q
 -}
mainPrissonTransport :: IO ()
mainPrissonTransport = do
  n <- readLn :: IO Int
  a <- fmap (map ((\[p, q] -> (p, q)) . map read . words) . lines) (getLine >> getContents)
  print $ prissonTrassport n a


-- SUBSTRING SEARCH -------------------------------------------------------------------------------
  
{-|
 - foreachWith takes a list, and a value that can be modified by the function, and
 - it returns the modified value after mapping the function over the list.  
 -}
foreachWith :: (Foldable t, Monad m) => t a -> b -> (a -> b -> m b) -> m b
foreachWith xs v f =
  foldM (flip f) v xs


{-|
 - It is a helper function that takes in a char vector and a mutable vector and two indices
 - where i represents the present index for which we are serching the jump value and j which
 - represents the value we will use to fill the jump val.
 -
 - if the value at both places is the same then whenever we see the pattern we can safely
 - go to the next index w.r.t j as the rest is the prefix that will already be matched
 - abcdax   jmp[i] can be 1.
 - ^   ^ -> 
 - j   i    
 - if pat[j] == pat[i] then jmp[i] = j + 1; i++; j++
 -
 - if pat[j] /= pat[i] then we need to start from the top.
 - if j == 0 then jmp[i] = 0; i++
 -
 - if all else fails we will look for the value at one less.
 - otherwise = j = jmp[j - 1]
 -}
searchUntil :: V.Vector Char -> MV.MVector s Int -> Int -> Int -> ST s Int
searchUntil pat mv i j
  | pat V.! j == pat V.! i = MV.write mv i (j + 1) >> return (j + 1)
  | j == 0 = MV.write mv i 0 >> return j
  | otherwise = MV.read mv (j - 1) >>= searchUntil pat mv i


{-|
 - Given a Char vector (i.e. our pattern) we will construct a jump vector out of it. Vector
 - that will govern our behaviour when we search.
 -}
build :: V.Vector Char -> V.Vector Int
build pat = runST $  do
  let n = V.length pat
  mv <- MV.replicate n 0 :: ST s (MV.MVector s Int)
  -- for ever i call the searchUntil function.
  _ <- foreachWith [1..n-1] 0 $ searchUntil pat mv
  V.freeze mv


{-|
 - Given a search string a pattern string and jump table and the index of the pattern we
 - are considering (0 when invoked) returns weither the pattern is in the search string or not.
 -
 - cases:
 - if we are at end of the pattern then it is present.
 - if pat[j] == character of search string under consideration then just increment j.
 - if we failed to match then there are 2 cases
 -   j is not 0 then we go one index back in our jump table and read the value of j there
 -   and start matching at that point.
 -   j is zero we start all over again.
 -}
search :: String -> V.Vector Char -> V.Vector Int -> Int -> Bool
search [] _ jmp j = j == V.length jmp
search (x:xs) pat jmp j
  | j == V.length jmp = True
  | pat V.! j == x = search xs pat jmp (j + 1)
  | j /= 0 = search (x:xs) pat jmp (jmp V.! (j - 1))
  | otherwise = search xs pat jmp j


{-|
 - The KMP-string searching algorithm. Given a searchString (possibly infinite) and a pattern
 - returns "YES" if the pattern exists in the string else "NO". Runs in time O(m + n) where
 - m is the size of the search string and n is the size of pattern we are searching.
 -}
kmp :: String -> String -> String
kmp str pat =
  let
    vecPat = V.fromList pat
    jmpTable = build vecPat
  in
    if search str vecPat jmpTable 0 then "YES" else "NO"

{-|
 - In 1974, a very fast string searching method was proposed by the name of KMP algorithm with
 - linear run-time complexity. Your task here is to code this (or any similar) algorithm in a
 - functional language.
 - 
 - Given two strings text and pat, find whether pat exists as a substring in text.
 - 
 - Input 
 - First line will contain an integer, T, which represents total number of test cases. Then T test
 - cases follow. Each case will contains two lines each containing a string. First line will contain
 - text while the second line will contain pat.
 - 
 - Output 
 - For each case print YES if pat is a substring of text otherwise NO.
 - 
 - Constraints 
 - 1 ≤ T ≤ 10 
 - 1 ≤ |pat| ≤ |text| ≤ 100000 
 - All characters in text and pat will be lowercase latin character ('a'-'z').
 -}
mainKMP :: IO ()
mainKMP =
  getLine >> getContents >>=
    putStr . unlines . map (uncurry kmp . (\[x,y] -> (x,y))) . chunksOf 2 . lines


-- JOHN AND FENCES --------------------------------------------------------------------------------

{-|
 - Given a fence structure calculates the maximum salvagable rectangular block of the fence.
 -}
maxFence :: [Int] -> Int
maxFence [] = 0
maxFence [x] = x
maxFence xs =
  let
    -- if solution includes the minimum element then it has to include the whole fence
    -- provided in the recursive call (i.e. pVal * length xs)
    (pVal, pIdx) = findMin xs
    -- spliting around the minimum element
    lo = take pIdx xs
    hi = drop (pIdx + 1) xs
  in
    -- return maximum of lower-half upper-half and fence structure if we included the shortest
    -- fence
    maximum [maxFence lo, maxFence hi, pVal * length xs]


{-| Returns the minimum in a list with it's index -}
findMin :: [Int] -> (Int, Int)
findMin xs =
  minimum $ zip xs [0..]

{-|
 - John's house has bizarre fencing. There are N fences. Though the contiguous fences have the
 - constant width of 1 unit but their height varies. Height of these fences is represented by array
 - H = [h1, h2... hN].
 -
 - John loves his fences but has to finally bow down to his wife's repeated requests of replacing
 - them with the regular fences. Before taking them down, John wants to keep some part of the fences
 - as souvenir. He decides to carve out the largest rectangular area possible where the largest
 - rectangle can be made of a number of contiguous fence. Note that sides of the rectangle should be
 - parallel to X and Y axis.
 -
 - Let's say there are 6 fences, and their height is, H = [2, 5, 7, 4, 1, 8]. Then they can be
 - represented as
 -
 -                    __
 - 8         __      |  |
 - 7        |  |     |  |
 - 6      __|  |     |  |
 - 5     |  |  |__   |  |
 - 4     |  |  |  |  |  |
 - 3   __|  |  |  |  |  |
 - 2  |  |  |  |  |__|  |
 - 1  |__|__|__|__|__|__|
 -     h1 h2 h3 h4 h5 h6
 - Some possible carvings are as follow:
 -
 - If we carve rectangle from h1, h2 and h3 then we can get the max area of 2x3 = 6 units.
 - If we carve rectangle from h3, h4, h5 and h6, then max area is 4x1 = 4 units.
 - If we carve rectangle from h2, h3 and h4, then max area is 4x3 = 12, which is also the most
 - optimal solution for this case.
 -
 - Input
 - First line will contain an integer N denoting the number of fences. It will be followed by a line
 - containing N space separated integers, h1 h2 ... hN, which represents the height of each fence.
 -
 - Output
 - Print the maximum area of rectangle which can be carved out.
 -
 - Note
 -
 - Constraints
 - 1 ≤ N ≤ 105
 - 1 ≤ hi ≤ 104
 -}
mainFence :: IO ()
mainFence =
  getLine >> getLine >>= print . maxFence . map read . words


-- RANGE MINIMUM QUERY ----------------------------------------------------------------------------

-- | two ints representing a range
type Range =
  (Int, Int)

-- | how does one range overlaps other
data Overlap
  = No
  | Partial
  | Complete
  deriving (Eq, Show)


-- | Given two Trees merges them together under a node
-- If only one tree is provided returns that tree directly
merge :: [Tree (Range, Int)] -> Tree (Range, Int)
merge [l] = l
merge [left@(Node ((lo, _), vLo) _ _), right@(Node ((_, hi), vHi) _ _)] =
  Node ((lo, hi), min vLo vHi) left right
merge _ = error "Can't merge these many trees"


-- | constructs a segment tree from a list of ints with their range
fromList' :: [Tree (Range, Int)] -> Tree (Range, Int)
fromList' [] = Empty
fromList' [x] = x
fromList' xs =
  fromList' . map merge $ chunksOf 2 xs


-- | Constructs a segment tree from a list of integers
segTreeFromList :: [Int] -> Tree (Range, Int)
segTreeFromList =
  fromList' . zipWith (curry singleton) (map (\x -> (x, x)) [0..])


-- | How much does the first range overlap with the second range
overlaps :: Range -> Range -> Overlap
overlaps (xlo, xhi) (ylo, yhi)
  | xlo <= ylo && xhi >= yhi = Complete
  | xhi < ylo || xlo > yhi = No
  | otherwise = Partial


-- | Given a segment tree and a range find the minimum number in that range
rangeQ :: Tree (Range, Int) -> Range -> Int
rangeQ Empty _ = error "No cookie for you"
rangeQ (Node (rx, vx) left right) range =
  case range `overlaps` rx of
    Complete -> vx
    Partial -> min (rangeQ left range) (rangeQ right range)
    No -> maxBound :: Int


{-|
 - Range Minimum Query (RMQ) is a set of problems which deals with finding a property (here minimum)
 - of a range. Segment Tree can be very helpful when solving with such problems. A segment tree is
 - a tree like data structure which is used to store the information about intervals.
 -
 - You are given a array of N integers, arr[0], arr[1], .., arr[(N-1)]. And you are given a list
 - of ranges. For each range, (l, r) you have to find the minimum value between range arr[l],
 - arr[l+1], arr[l+2], .., arr[r].
 -
 - Input
 - First line will contain two integers, N M, length of array and number of queries. Then in next
 - line, there are N space separated integers which represent the array, arr[0], arr[1], ..,
 - arr[N-1]. Then M line follows. Each M line will contain two integers, l r, representing a range.
 -
 - Output
 - For each range, (l, r), you have to print the minimum integer in subarray arr[l], arr[l+1], ..,
 - arr[r] in separate line.
 -
 - Constraints
 - 1 <= N, M <= 105
 - -105 <= arr[i] <= 105 , where 0 <= i < N
 - 0 <= l <= r < N
 -}
mainMinRQ :: IO ()
mainMinRQ = do
  as <- fmap (map read . words) (getLine >> getLine)
  qs <- fmap (map ((\[x, y] -> (x, y)) . map read . words) . lines) getContents
  putStrLn .unlines $ map (show . rangeQ (segTreeFromList as)) qs


-- MAIN -------------------------------------------------------------------------------------------

functionalStructures :: [(String, IO ())]
functionalStructures =
  [ ("swapNodes", mainSwapNodes)
  , ("listGCD", mainListsAndGCD)
  , ("validBST", mainValidBST)
  , ("prissonTrassport", mainPrissonTransport)
  , ("substringSearch", mainKMP)
  , ("jhonAndFence", mainFence)
  , ("minRangeQuery", mainMinRQ)
  ]

