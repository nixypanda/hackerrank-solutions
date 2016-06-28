{-|
 - Sorting related challenges
 -}

import qualified Data.Vector as Vec
import qualified Data.Array as Arr

-- operator imports
import Data.Vector ((!))

-- BINARY SEARCH ALGORITHM ------------------------------------------------------------------------

{-|
 - Given a sorted Vector find the index of the given number.
 -}
bSearch :: Ord a => Vec.Vector a -> a -> Int -> Int -> Int
bSearch ar val lo hi
  | lo > hi          = -1
  | val < (ar ! mid) = bSearch ar val lo (mid - 1)
  | val > (ar ! mid) = bSearch ar val (mid + 1) hi 
  | otherwise        = mid
  where mid = lo + (hi - lo) `div` 2


-- INSERTION SORT: PART 1 (MODIFIED TO ADJUST TO FUNCTIONAL PARADIGM) -----------------------------

{-|
 - Insert a given element into a sorted list
 -}
insert :: (Ord a) => a -> [a] -> [a]
insert x []     = [x]
insert x (y:ys) = if x < y then x : y : ys else y : insert x ys


-- INSERTION SORT: PART 2 (MODIFIED TO ADJUST TO FUNCTIONAL PARADIGM) ------------------------------

{-|
 - The complete insertion sort algorithm
 -}
insertionSort :: (Ord a) => [a] -> [a]
insertionSort = foldr insert []


-- CORRECTNESS OF LOOP INVARIANT (NOT AVAILABLE) :( ------------------------------------------------


-- QUICKSORT: 1 PARTITIONING -----------------------------------------------------------------------

{-|
 - Given an element and a list partition the list around that pivot.
 -}
partition :: (Ord a) => a -> [a] -> [a]
partition x xs = [y | y <- xs, y < x] ++ [x] ++ [y | y <- xs, y > x]


-- QUICKSORT: 2 SORTING ----------------------------------------------------------------------------

{-|
 - Sort the given list using quick sort sorting algorithm
 -}
qSort :: (Ord a) => [a] -> [a]
qSort []     = []
qSort [x]    = [x]
qSort (x:xs) = qSort [y | y <- xs, y < x] ++ x : [y | y <- xs, y == x] ++ qSort [y | y <- xs, y > x]


-- COUNTING SORT I --------------------------------------------------------------------------------

{-|
 - Given a list of integers, can you count and output the number of times each value appears?
 - Hint: There is no need to sort the data, you just need to count it.
 -}
-- the map here is creating the occurence of every element
-- accumArray will calculate the repeated occurences
-- elems gets the values
count :: (Arr.Ix i, Num e, Num i) => i -> [i] -> [e]
count maxNum = Arr.elems . Arr.accumArray (+) 0 (0, maxNum) . map (\x -> (x, 1))


-- COUNTING SORT II -------------------------------------------------------------------------------

{-|
 - Given an unsorted list of integers, output the integers in order.
 -}
countSort :: (Arr.Ix i, Num i) => i -> [i] -> [i]
countSort maxNum xs = concatMap (\(x, y) -> replicate y x) $ count xs
  -- slightliy modified count (elems -> assocs)
  where count = Arr.assocs . Arr.accumArray (+) 0 (0, maxNum) . map (\x -> (x, 1))


-- COUNTING SORT III ------------------------------------------------------------------------------

{-|
 - For every value i from o to 99, can you output L, the number of elements that are less than
 - or equal to i?
 -}
-- does a count (basicaly frequency distro then a cumulative sum)
-- reusing the previously defind count function
accumCountSort :: (Arr.Ix i, Num i, Num a) => a -> [i] -> [a] 
accumCountSort len xs = scanl1 (+) $ count 99 xs


-- FIND THE MEDIAN --------------------------------------------------------------------------------

{-|
 - Given a list of numbers find the median of the list.
 -}
-- piggyback on quick sort only sorting the porition that is required
quickSelect :: Ord a => [a] -> Int -> a
quickSelect (x:xs) mid
  | leftLen == mid = x
  | leftLen <  mid = quickSelect (filter (> x) xs) (mid - leftLen - 1)
  | otherwise      = quickSelect lesser mid
  where
    leftLen = length lesser
    lesser  = filter (<= x) xs


-- SHERLOCK AND WATSON ----------------------------------------------------------------------------

{-|
 - John Watson performs an operation called a right circular rotation on an array of integers.
 -
 - Watson performs this operation k times. To test Sherlock's ability to identify the current
 - element at a particular position in the rotated array, Watson asks q queries, where each query
 - consists of a single integer for which you must print the element at that index in the rotated
 - array (i.e., the value of ).
 -}
sherlockReplies :: Int -> Int -> [a] -> [Int] -> [a]
sherlockReplies n k xs = map (\i -> vec ! ((i - k) `mod` n)) where vec = Vec.fromList xs


-- INSERTION SORT ADVANCED ANALYSIS ---------------------------------------------------------------

{-
 - Count the number of split inversions between two sorted arrays. Tracking the
 - current length of xs as (length xs) would mean linear time operation leading
 - to a quadratic merge.
 -}
-- similar to the merge sub-routine but also returns the split inversion count along
-- with the merged array.
-- IDEA: the idea is that when copying over element from the right list, the element being
-- copied over forms split inversions with all the remaining elements in the left list.
-- lxs : maintains the length of the remaining xs
splitInvs :: (Ord a) => Integer -> [a] -> [a] -> ([a], Integer)
splitInvs lxs [] [] = ([], 0)
splitInvs lxs xs [] = (xs, 0)
splitInvs lxs [] ys = (ys, 0)
splitInvs lxs (x:xs) (y:ys)
  | x < y     = (x:rxs, srxs)
  | x > y     = (y:rys, lxs + srys)
  | otherwise = (x:y:rxy, srxy)
    where
      (rxs, srxs) = splitInvs (lxs - 1) xs (y:ys)
      (rys, srys) = splitInvs lxs (x:xs) ys
      (rxy, srxy) = splitInvs (lxs - 1) xs ys

{-
 - Count the total number of inversions in a given array
 -}
-- simply like the mergesort algorithm but also returns the addition of
-- left half inversions count + right half inversions count + split inversion count.
inversionCount :: (Ord a) => [a] -> ([a], Integer)
inversionCount []  = ([], 0)
inversionCount [x] = ([x], 0)
inversionCount zs  = (szs, nxs + nys + nzs)
  where
    half       = length zs `div` 2
    xs         = take half zs
    ys         = drop half zs
    (sxs, nxs) = inversionCount xs
    (sys, nys) = inversionCount ys
    (szs, nzs) = splitInvs (fromIntegral half) sxs sys

