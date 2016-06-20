{-
 - Solutions to implementation challanges in algorithms track on hacker-rank.
 -}

import qualified Data.List as List
import qualified Data.ByteString as ByteStr
import qualified Data.ByteString.Char8 as ByteStrCh
import qualified Data.Vector as Vec
import Data.Vector ((!))
import qualified Data.Char as Ch

-- GENERAL HELPERS --------------------------------------------------------------------------------

{-|
 - Cheates chunks of size k for the given list
 - e.g. chunksOf 2 [1..5] = [[1, 2], [3, 4], [5]]
 -}
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = ys : chunksOf k zs where (ys, zs) = splitAt k xs


-- IS THE PROFESSOR ANGRY -------------------------------------------------------------------------

{-|
 - Finds out if the professor gets angry enough to cancel the class?
 - He gets angry if less than k student reach on time. <=0 => before/on Time
 -}
isProfAngry :: (Ord a, Num a) => [a] -> Int -> String
isProfAngry arr k = if length (filter (<= 0) arr) < k then "YES" else "NO"


-- THE LARGEST DECENT NUMBER ----------------------------------------------------------------------

{-|
 - Finds the largest decent number of length n.
 - A decent number is one which:
 -
 - - digits can only be 3's and/or 5's.
 - - The number of 3's it contains is divisible by 5.
 - - The number of 5's it contains is divisible by 3.
 -}
largestDecent :: Int -> String
largestDecent len = head (map fivesAndThrees tees)
  where
    tees = filter (\x -> (len - x) `mod` 3 == 0 && x `mod` 5 == 0) [0..(len + 1)]
    fivesAndThrees t = concat (replicate (len - t) "5") ++ concat (replicate t "3")


-- HEIGHT OF A UTOPIAN TREE -----------------------------------------------------------------------

{-|
 - Calculates the length of the utopian tree at a given snapshot in time
 - The Utopian Tree goes through 2 cycles of growth every year. Each spring, it doubles in height.
 - ach summer, its height increases by 1 meter.
 -}
utopHeight :: (Integral a, Integral b) => a -> b
utopHeight 0 = 1
utopHeight n = if n `mod` 2 /= 0 then 2 * utopHeight (n - 1) else utopHeight (n - 1) + 1


-- EVEN DIVISORS OF A NUMBER ----------------------------------------------------------------------

{-| Digits that make up n and also evenly divide it.  -}
evenDivisors num = reverse $ filter (\x -> x /= 0 && num `mod` x == 0) (digs num)
  where
    digs 0 = []
    digs n = (n `mod` 10) : digs (n `div` 10)


-- SHERLOCK AND SQUARES ---------------------------------------------------------------------------

{-| Number of squares in a given limit -}
squares lo hi = floor (hi**(1.0/2.0)) - ceiling (lo**(1.0/2.0)) + 1


-- SERVICE LANE -----------------------------------------------------------------------------------

{-|
 - Which vechicle can I squeeze through the service lane
 - width : the width of all the service lane points
 - start to end the points between which we check vechicale we can squeeze.
 -}
vechile width start end = minimum $ take (end - start + 1) (drop start width)


-- CUT THE STICKS ---------------------------------------------------------------------------------

{-|
 - Cut the sticks until you can cut no more. i.e.
 - Suppose we have six sticks of the following lengths:
 - 5 4 4 2 2 8
 - 
 - Then, in one cut operation we make a cut of length 2 from each of the six sticks. For the
 - next cut operation four sticks are left (of non-zero length), whose lengths are the following: 
 - 3 2 2 6
 -}
cut [] = []
cut array = length cutof : cut cutof
  where cutof = filter (/= 0) [x - minimum array | x <- array]


-- CHOCOLATE FEAST --------------------------------------------------------------------------------

{-|
 - Little Bob loves chocolate, and he goes to a store with $N in his pocket. The price of each
 - chocolate is $C. The store offers a discount: for every  wrappers M he gives to the store,
 - he gets one chocolate for free. How many chocolates does Bob get to eat?
 -}
-- get the initial number of choclates and then get how many
-- one can by using wrappers
choclates n c m = canBuy + fromwraps canBuy m
  where
    canBuy = n `div` c
    -- recursivly get choclates until can't get anymore
    fromwraps ws mws
      | ws < mws = 0
      | ws == mws = 1
      -- wrappers / (minimum wrappers) to get choclates
      -- then add the new wrappers to the remaining ones and ask for choclates again
      | otherwise = chocs + fromwraps ((ws `mod` mws) + chocs) mws where chocs = ws `div` mws


-- LISA'S WORKBOOK --------------------------------------------------------------------------------

{-|
 - Lisa just got a new math workbook. A workbook contains exercise problems, grouped into chapters.
 -
 - There are n chapters in Lisa's workbook, numbered from 1 to n.
 - The i-th chapter has t_i problems, numbered from 1 to t_i.
 - Each page can hold up to k problems. There are no empty pages or unnecessary spaces, so only the
 - last page of a chapter may contain fewer than k problems.
 - Each new chapter starts on a new page, so a page will never contain problems from more than one
 - chapter.
 - The page number indexing starts at 1.
 -
 - Lisa believes a problem to be special if its index (within a chapter) is the same as the page
 - number where it's located. Given the details for Lisa's workbook, can you count its number of
 - special problems?
 -}
specialProblems :: (Foldable t, Num a, Eq a, Enum a) => Int -> t a -> Int
specialProblems k probsPerCh = length $ filter (uncurry elem) lisa'sBook
  where
    problemsPerPage k n = chunksOf k [1..n]
    lisa'sBook = zip [1..] $ concatMap (problemsPerPage k) probsPerCh


-- THE GRID SEARCH --------------------------------------------------------------------------------

matchPattern :: [String] -> [String] -> Bool
matchPattern lx@(x:xs) ly@(y:ys) = and . zipWith List.isPrefixOf lx $ map (drop index) ly
  where index = ByteStr.length . fst $ ByteStr.breakSubstring (ByteStrCh.pack x) (ByteStrCh.pack y)

{-| find a 2-d pattern in a 2-D grid-}
isSubPattern :: [String] -> [String] -> Bool
isSubPattern _ []      = False
isSubPattern xs (y:ys) = List.isInfixOf (head xs) y && matchPattern xs (y:ys) || isSubPattern xs ys


-- CAVITY MAP -------------------------------------------------------------------------------------

{-|
 - Finds the cavities in a given grid.
 -
 - You are given a square map of size n * m. Each cell of the map has a value denoting its depth.
 - We will call a cell of the map a cavity if and only if this cell is not on the border of the map
 - and each cell adjacent to it has strictly smaller depth. Two cells are adjacent if they have a
 - common side (edge).
 -
 - You need to find all the cavities on the map and depict them with the uppercase character X.
 -}
cavityMap :: [String] -> [String]
cavityMap lx@(x:xs) = x : gridCavities 1 xs
  where
    -- creating a 2-d vector for fast value lookups
    grid :: Vec.Vector (Vec.Vector Int)
    grid = Vec.fromList $ map (Vec.fromList . (map Ch.digitToInt)) lx

    -- check if the given location is a cavit or not i.e. it's depth is strictly greater than
    -- the depth of the adjacent cells.
    isCavity :: Int -> Int -> Bool
    isCavity y x = ((grid!y)!x) > (maximum $ prev : next : same)
      where
        prev = grid ! (y - 1) ! x
        same = [p ! (x - 1), p ! (x + 1)] where p = grid ! y
        next = grid ! (y + 1) ! x

    -- takes a row and returns a new one in which 'X' replaces the cavity spots in that row
    rowWithCavities:: Int -> Int -> String -> String
    rowWithCavities _ _ [] = []
    rowWithCavities _ _ [x] = [x]
    rowWithCavities r c (x:xs) = (if isCavity r c then 'X' else x) : rowWithCavities r (c + 1) xs

    -- finds all the cavities in a given grid
    gridCavities :: Int -> [String] -> [String]
    gridCavities _ [] = []
    gridCavities _ [y] = [y]
    gridCavities r (y:ys) = ((head y) : rowWithCavities r 1 (tail y)) : gridCavities (r + 1) ys


-- CEASAR CIPHER ----------------------------------------------------------------------------------

{-|
 - Encrypt a sting using the ceasar cipher.
 - The cipher rotated every letter in a string by a fixed number K, making it unreadable by
 - his enemies. Given a string, s and a number k , encrypt s and print the resulting string.
 -}
ceasar :: Int -> String -> String
ceasar k = map shift
  where
    shiftChar base k ch = Ch.chr $ base + (Ch.ord ch - base + k) `mod` 26
    shift c = case Ch.generalCategory c of
      Ch.LowercaseLetter -> shiftChar 97 k c
      Ch.UppercaseLetter -> shiftChar 65 k c
      _                  -> c


-- LIBRARY FINE -----------------------------------------------------------------------------------

{-|
 - Calculate the fine on a book that is being returned to the libarary.
 - The fee structure is:
 - If the book is returned on or before the expected return date, no fine will be charged.
 - If the book is returned after the expected return day but still within the same calendar month
 - and year as the expected return date, fine = 15 x (days late).
 - If the book is returned after the expected return month but still within the same calendar year
 - as the expected return date, the fine = 500 x (months late).
 - If the book is returned after the calendar year in which it was expected, there is a fixed fine
 - of 10000.
 -}

fine :: (Int, Int, Int) -> (Int, Int, Int) -> Int
fine (ed, em, ey) (ad, am, ay) = if sum fines < 0 || null filterd then 0 else head filterd
  where 
    fines   = [10000 * signum (ay - ey), 500 * (am - em) , 15 * (ad - ed)]
    filterd = filter (> 0) fines

