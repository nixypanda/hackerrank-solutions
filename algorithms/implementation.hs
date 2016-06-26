{-
 - Solutions to implementation challanges in algorithms track on hacker-rank.
 -} 

-- qualified imports
import qualified Data.List as List
import qualified Data.ByteString as ByteStr
import qualified Data.ByteString.Char8 as ByteStrCh
import qualified Data.Vector as Vec
import qualified Data.Char as Ch
import qualified Data.Bits as Bits

-- operator imports
import Data.Vector ((!))
import Data.Bits ((.|.))


-- GENERAL HELPERS --------------------------------------------------------------------------------

{-|
 - Creates chunks of size k for the given list
 - e.g. chunksOf 2 [1..5] = [[1, 2], [3, 4], [5]]
 -}
-- recursivly split at the specified chunk size.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = ys : chunksOf k zs where (ys, zs) = splitAt k xs


{-|
 - Takes a list a generates all internally possible pairs.
 -}
-- take an element and pair it with all others then recursivly do it with the remaining elements.
pairs :: [a] -> [(a, a)]
pairs []     = []
pairs (x:xs) = map (\y -> (x, y)) xs ++ pairs xs


{-|
 - Convertes a string of 0'a or (inclusive) 1's to a valid Integer value
 -}
-- summation of 2 ^ (location of bit from right) for all locations -> decimal value
toDec :: String -> Integer
toDec = List.foldl' (\acc x -> acc * 2 + fromIntegral (Ch.digitToInt x)) 0


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
    tees             = filter (\x -> (len - x) `mod` 3 == 0 && x `mod` 5 == 0) [0..(len + 1)]
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
evenDivisors :: Integral a => a -> [a]
evenDivisors num = reverse $ filter (\x -> x /= 0 && num `mod` x == 0) (digs num)
  where
    digs 0 = []
    digs n = (n `mod` 10) : digs (n `div` 10)


-- SHERLOCK AND SQUARES ---------------------------------------------------------------------------

{-| Number of squares in a given limit -}
squares :: (RealFrac a, Floating a, Integral b) => a -> a -> b
squares lo hi = floor (hi**(1.0/2.0)) - ceiling (lo**(1.0/2.0)) + 1


-- SERVICE LANE -----------------------------------------------------------------------------------

{-|
 - Which vechicle can I squeeze through the service lane
 - width : the width of all the service lane points
 - start to end the points between which we check vechicale we can squeeze.
 -}
vechile :: (Ord a) => [a] -> Int -> Int -> a
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
cut :: (Ord a, Num a) => [a] -> [Int]
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
chocolates :: (Integral a) => a -> a -> a -> a
chocolates n c m = canBuy + fromwraps canBuy m
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
    grid = Vec.fromList $ map (Vec.fromList . map Ch.digitToInt) lx

    -- check if the given location is a cavit or not i.e. it's depth is strictly greater than
    -- the depth of the adjacent cells.
    isCavity :: Int -> Int -> Bool
    isCavity y x = ((grid!y)!x) > maximum (prev : next : same)
      where
        prev = grid ! (y - 1) ! x
        same = [p ! (x - 1), p ! (x + 1)] where p = grid ! y
        next = grid ! (y + 1) ! x

    -- takes a row and returns a new one in which 'X' replaces the cavity spots in that row
    rowWithCavities:: Int -> Int -> String -> String
    rowWithCavities _ _ []     = []
    rowWithCavities _ _ [x]    = [x]
    rowWithCavities r c (x:xs) = (if isCavity r c then 'X' else x) : rowWithCavities r (c + 1) xs

    -- finds all the cavities in a given grid
    gridCavities :: Int -> [String] -> [String]
    gridCavities _ []     = []
    gridCavities _ [y]    = [y]
    gridCavities r (y:ys) = (head y : rowWithCavities r 1 (tail y)) : gridCavities (r + 1) ys


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

-- MANASA AND STONES ------------------------------------------------------------------------------

{-|
 - Manasa is out on a hike with friends. She finds a trail of stones with numbers on them. She
 - starts following the trail and notices that two consecutive stones have a difference of either a
 - or b. Legend has it that there is a treasure trove at the end of the trail and if Manasa can
 - guess the value of the last stone, the treasure would be hers. Given that the number on the
 - first stone was 0, find all the possible values for the number on the last stone.
 -
 - Note: The numbers on the stones are in increasing order.
 -}
stones :: (Ord a, Num a) => a -> a -> a -> [a]
stones n a b =
  if lo == hi
  then [(n - 1) * lo]
  else takeWhile (<= (n - 1) * hi) $ scanl (\acc x -> acc - lo + hi) ((n - 1) * lo) [1..]
    where
      lo = min a b
      hi = max a b


-- ACM ICPC TEAM ----------------------------------------------------------------------------------

{-|
 - You are given a list of N people who are attending ACM-ICPC World Finals. Each of them are
 - either well versed in a topic or they are not. Find out the maximum number of topics a 2-person
 - team can know. And also find out how many teams can know that maximum number of topics.
 -
 - Note Suppose a, b, and c are three different people, then (a,b) and (b,c) are counted as two
 - different teams.
 -}

acmicpcTeam :: [String] -> (Int, Int)
acmicpcTeam xs = (maximum teamKnowHow, length $ filter (== maximum teamKnowHow) teamKnowHow)
  where teamKnowHow = map (\(x, y) -> Bits.popCount (x .|. y)) . pairs $ map toDec xs


-- EXTRA LONG FACTORIALS --------------------------------------------------------------------------

{-| Given a number n return it's factorial -}
factorial :: Integer -> Integer
factorial n = product [1..n]


-- TAUM AND B'DAY ---------------------------------------------------------------------------------

{-|
 - Taum is planning to celebrate the birthday of his friend, Diksha. There are two types of
 - gifts that Diksha wants from Taum: one is black and the other is white. To make her happy,
 - Taum has to buy  number of black gifts and  number of white gifts.
 -
 - The cost of each black gift is pb units.
 - The cost of every white gift is pw units.
 - The cost of converting each black gift into white gift or vice versa is pc units.
 - Help Taum by deducing the minimum amount he needs to spend on Diksha's gifts.
 -}
price :: (Ord a, Num a) => a -> a -> a -> a -> a -> a
price w b pw pb pc = minimum [w * pw + b * pb, w * (pb + pc) + b * pb, w * pw + b * (pw + pc)]


-- TIME IN WORDS ----------------------------------------------------------------------------------

{-|
 - Given time in numbers conver it to words.
 - e.g 5:00 -> 5 o' clock
 -}
inWords :: Int -> Int -> String
inWords h m
    | m == 0  = lookup !! h ++ " o' clock"
    | m == 1  = lookup !! m ++ " minute past " ++ lookup !! h
    | m == 15 = "quarter past " ++ lookup !! h
    | m == 30 = "half past " ++ lookup !! h
    | m == 45 = "quarter to " ++ lookup !! ((h + 1) `mod` 24)
    | m == 59 = "one minute to " ++ lookup !! ((h + 1) `mod` 24)
    | m < 30  = lookup !! m ++ " minutes past " ++ lookup !! h
    | m < 59  = lookup !! (60 - m) ++ " minutes to " ++ lookup !! ((h + 1) `mod` 24)
    where
        lookup = [ "zero"
                 , "one"
                 , "two"
                 , "three"
                 , "four"
                 , "five"
                 , "six"
                 , "seven"
                 , "eight"
                 , "nine"
                 , "ten"
                 , "eleven"
                 , "twelve"
                 , "thirteen"
                 , "fourteen"
                 , "fifteen"
                 , "sixteen"
                 , "seventeen"
                 , "eighteen"
                 , "ninteen"
                 , "twenty"
                 , "twenty one"
                 , "twenty two"
                 , "twenty three"
                 , "twenty four"
                 , "twenty five"
                 , "twenty six"
                 , "twenty seven"
                 , "twenty eight"
                 , "twenty nine"
                 ]


-- MODIFIED KAPREKAR ------------------------------------------------------------------------------

{-|
 - A modified Kaprekar number is a positive whole number  with  digits, such that when we split
 - its square into two pieces - a right hand piece  with d digits and a left hand piece that
 - contains the remaining d-1 digits, the sum of the pieces is equal to the original number
 -
 - Here's an explanation from Wikipedia about the ORIGINAL Kaprekar Number (spot the difference!)
 - In mathematics, a Kaprekar number for a given base is a non-negative integer, the
 - representation of whose square in that base can be split into two parts that add up to the
 - original number again. For instance, 45 is a Kaprekar number, because 45² = 2025 and 20+25 = 45
 -}

modifiedKaprekar :: [Integer]
modifiedKaprekar = filter isKaprekar [1..]
  where
    isKaprekar n = nSquare `div` split + nSquare `mod` split == n
      where
        nSquare = toInteger n * n
        split   = 10 ^ (ceiling (1.000001 + logBase 10 (fromIntegral nSquare)) `div` 2) :: Integer


-- ENCRYPTION -------------------------------------------------------------------------------------

{-|
 - Encrypt english text by making a grid of it out of it of sqrt(l) length then reading
 - it column by column.
 -}
encryption :: String -> [String]
encryption s = List.transpose $ chunksOf n s where n = (ceiling . sqrt) . fromIntegral $ length s


-- LARRY'S ARRAY ----------------------------------------------------------------------------------

{-
 - Larray's has a permutation of N numbers A whose unique elements renge from 1 to N. He wants it
 - to be sorted so delegates the task to his robot. The robot can perform the following operation
 - as many times as it wants.
 -
 - Choose any  consecutive indices and rotate their elements:
 - e.g. ABC -> BCA -> CAB ..
 -
 - Given an array check if the robot can sort it or not.
 -}
canRobotSort :: (Ord a) => [a] -> Bool
canRobotSort xs = snd (inversionCount xs) `mod` 2 == 0


-- NEW YEAR CHAOS ---------------------------------------------------------------------------------

{-|
 - It's New Year's Day and everyone's in line for the Wonderland rollercoaster ride!
 -
 - There are n people queued up, and each person wears a sticker indicating their initial position
 - in the queue (i.e.:1, 2...  with the first number denoting the frontmost position).
 -
 - Any person in the queue can bribe the person directly in front of them to swap positions. If
 - two people swap positions, they still wear the same sticker denoting their original place in
 - line. One person can bribe at most two other persons.
 -
 - That is to say, if n = 8 and Person 5  bribes Person 4 , the queue will look like this: .. 5, 4..
 - Fascinated by this chaotic queue, you decide you must know the minimum number of bribes that
 - took place to get the queue into its current state!
 -}
newYearChaos :: [Int] -> String
newYearChaos q = if isTooChaotic 1 q then "Too chaotic" else show . snd $ inversionCount q
  where
    isTooChaotic :: Int -> [Int] -> Bool
    isTooChaotic _ []     = False
    isTooChaotic i (x:xs) = i - x < (-2) || isTooChaotic (i + 1) xs


---------------------------------------------------------------------------------------------------
