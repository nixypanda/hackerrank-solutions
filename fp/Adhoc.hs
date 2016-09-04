{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Adhoc
  ( adhoc
  , factors
  )
  where

import Control.Monad (replicateM_, forM_)
import Control.Monad.ST (ST)
import Data.List
  ( foldl1
  , scanl1
  , sort
  , sortBy
  )
import Data.Ord (comparing)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Maybe as M
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV


-- HELPERS ---------------------------------------------------------------------------------------

factors :: Integral a => a -> [a]
factors n =
  let
    lows = filter ((== 0) . mod n) [1..truncate . (sqrt :: Double -> Double) $ fromIntegral n]
  in
    lows ++ reverse (map (div n) lows)


{-|
 - Calculate the lcm of a given list of numbers.
 -}
lcm' :: [Int] -> Int
lcm' = foldl1 lcm


{-|
 - Given any list rotate it by given n amount
 -}
rotate :: Int -> [a] -> [a]
rotate n ys =
  take (length ys) $ drop n (cycle ys)


{-|
 - Generate all iterations of a given list.
 -}
rotations :: [a] -> [[a]]
rotations xs =
  [ rotate i xs | i <- [1..(length xs)] ]


{-|
 - Break up a number into it's constituent digits
 -}
digs :: Integral a => a -> [a]
digs 0 = []
digs x =
  x `mod` 10 : digs (x `div` 10)


{-|
 - Check if the number contains zero
 -}
hasZero :: Integral a => a -> Bool
hasZero =
  elem 0 . digs


{-|
 - Return a list of numbers while droping from the left
 -}
fromLeft :: Integral a => a -> [a]
fromLeft =
  takeWhile (> 0) . iterate (`div` 10)


{-|
 - Return a list of numbers while droping the rightmost digit at every step
 -}
fromRight :: Integral a => a -> [a]
fromRight =
  foldl (\acc x -> ((if null acc then 0 else head acc)*10 + x) : acc) [] . reverse . digs


{-|
 - Genrate an infinite list of prime numbers
 -}
primes :: Integral a => [a]
primes =
  let
    isPrime [] _ = error "Huh?"
    isPrime (p:ps) n =
      p*p > n || n `rem` p /= 0 && isPrime ps n

    primes' =
      7 : filter (isPrime primes') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])
  in
    2 : 3 : 5 : primes'


-- JUMPING BUNNIES -------------------------------------------------------------------------------

{-|
 - Bunnies are very cute animals who likes to jump a lot. Every bunny has his own range of jump.
 - Lets say there are N bunnies and ith  bunny jumps j_i units. Consider a 1-D plane, where
 - initially bunnies are at 0. All of them starts jumping in forward direction.
 -
 - For example, kth consider the case of 0 bunny. Initially he is at j_k. After first jump, he
 - will be at point j_k. After second, he will be at 2* j_k and so on. After m jump, he will be at
 - point m * j_k .
 -
 - Two bunnies can only meet each other when they are on the ground. When on the ground, a bunny
 - can wait any amount of time. Being a social animal, all of them decide to meet at the next
 - point where all of them will be on the ground. You have to find the nearest point where all the
 - bunnies can meet.
 -
 - For example, if there are N = 3 bunnies where j_1 = 2, j_2 = 3, j_3 = 4. Nearest point where
 - all bunnies can meet again is at 12. First bunny has to jump six times, for second it is 4 times
 - and for third it is 3 times.
 -
 - Help bunnies to find the nearest point where they can meet again.
 -}
mainJumpingBunnies :: IO ()
mainJumpingBunnies = do
  _ <- readLn :: IO Int
  getLine >>= print . lcm' .  map (read :: String -> Int) . words


-- ROTATE STRING ----------------------------------------------------------------------------------

{-|
 - Scturtle likes strings very much. He is getting bored today, because he has already completed
 - this week's task and doesn't have anything else to do. So he starts left-rotating a string. If
 - the length of the string is n, then he will rotate it n times and note down the result of each
 - rotation on a paper.
 -
 - For a string S = s1,...sn, n rotations are possible. Let's represent these rotations by r1..rn.
 - Rotating it once will result in string r1 = s2s3...sns1, rotating it again will result in string
 - r2 = s3s4...sns1s2 and so on. Formally, i rotation will be equal to ri = s{i+1}...s{n-1}sns1..si.
 - Note that rn = S.
 -
 - Your task is to display all n rotations of string S.
 -
 - For example, if S = abc then it has 3 rotations. They are r1 = bca, r2 = cab and r3 = abc.
 -}
mainRotations :: IO ()
mainRotations =
  getContents >>= putStrLn . unlines . map (unwords . rotations) . tail . lines


-- REMOVE DUPLICATES ------------------------------------------------------------------------------

{-|
 - Taks a list of orderable items and returns a new list of uniques preserving the first occurences
 - order.
 -}
reduction :: (Ord a) => S.Set a -> [a] -> [a]
reduction _ []     = []
reduction seen (x:xs) =
  if S.member x seen
    then reduction seen xs
    else x : reduction (S.insert x seen) xs

{-|
 - You are given a string, str, of length N consisting of lowercase letters of alphabet. You have
 - to remove all those characters from str which have already appeared in it, i.e., you have to
 - keep only first occurance of each letter.
 -}
mainReduction :: IO ()
mainReduction =
  getLine >>= putStrLn . reduction S.empty


-- HUGE GCD ---------------------------------------------------------------------------------------

{-|
 - Computes GCD of two numbers modulo 10^9+7.
 -}
gcd' :: Integer -> Integer -> Integer
gcd' a b =
  gcd a b `mod` 1000000007

{-|
 - Gayasen has received a homework assignment to compute the greatest common divisor of the two
 - positive integers A and B. Since the numbers are quite large, the professor provided him with N
 - smaller integers whose product is A, and M integers with product B. He would like to verify
 - result, so he has asked you to write a program to solve his problem. But instead of printing
 - complete answer you have to print answer modulo 109+7.
 -}
mainGCD :: IO ()
mainGCD = do
  a <- fmap (product . map read . words) (getLine >> getLine)
  b <- fmap (product . map read . words) (getLine >> getLine)
  print $ gcd' a b


-- KUNDU AND BUBBLE WRAP --------------------------------------------------------------------------

{-|
 - Expected time it will take to hit n targets if you pick target at random (with repetation).
 -}
probs :: Double -> Double
probs n =
  sum $ map (n /) [1..n]

{-|
 - Kundu has a Bubble Wrap and like all of us she likes popping it. The Bubble wrap has dimensions
 - NxM, i.e. it has N rows and each row has M cells which has a bubble. Initially all bubbles in
 - filled with air and can be popped.
 -}
mainBubbleWrap :: IO ()
mainBubbleWrap =
  getLine >>= print . probs . (\[x, y] -> x * y) . map read . words


-- MISSING NUMBERS (FP) ---------------------------------------------------------------------------

-- Slightly complicated as I am using this in the algo domain as well where every ounce of speed
-- was required (Not really)


{-|
 - Given two lists returns the list of numbers that are in the first one but not in the second
 - one in sorted order. (See Constraints in mainMissingNums)
 -}
missingNums :: [Int] -> [Int] -> Int -> [Int]
missingNums xs ys minx =
  let
    vals = UV.zip (UV.enumFromN minx 101) (UV.create $ freqDiff xs ys minx)
  in
    UV.toList . UV.map fst $ UV.filter (\(_, y) -> y /= 0) vals

{-|
 - Creates a mutable vector of size 101 which contains the frequency difference between the b list
 - and the a list. (See Constraints in mainMissingNums)
 -}
freqDiff :: [Int] -> [Int] -> Int -> ST s (MUV.MVector s Int)
freqDiff xs ys minx = do
  v <- MUV.replicate 101 0
  forM_ xs $ \x -> MUV.modify v (+ 1) (x - minx)
  forM_ ys $ \y -> MUV.modify v (subtract 1) (y - minx)
  return v


{-
 - Sometimes you need to compare lists of number, but sorting each one normally will take too much
 - time. Instead you can use alternative methods to find the differences between each list.
 -
 - Challenge:
 - Numeros The Artist was arranging two identical lists A and B into specific orders. The
 - arrangements of the two arrays were random, Numeros was very proud of his arrangements.
 - Unfortunately, some numbers got left out of List A. Can you find the missing numbers from A
 - without messing up his order?
 -
 - Details:
 - There are many duplicates in the lists, but you need to find the extra numbers, i.e. B - A.
 - Print the numbers in numerical order. Print each missing number once, even if it is missing
 - multiple times. The numbers are all within a range of 100 from each other.
 -
 - Constraints
 - 1<= n,m <= 200000
 - -10000 <= x <= 10000 , x ∈ B
 - Xmax - Xmin < 101
-}
mainMissingNums :: IO ()
mainMissingNums = do
  a <- fmap (map (fst . M.fromJust . BC.readInt) . BC.words) (BS.getLine >> BS.getLine)
  b <- fmap (map (fst . M.fromJust . BC.readInt) . BC.words) (BS.getLine >> BS.getLine)
  putStr . unwords . map show $ missingNums b a (minimum b)


-- COMMON DIVISORS --------------------------------------------------------------------------------

{-|
 - Given two numbers returns the number of common divisors between them
 -}
cDivs :: Int -> Int -> Int
cDivs x y =
  S.size . S.fromList . factors $ gcd x y

{-|
 - Mario and Luigi earn points in their steps to save the Princess Peach from a dragon. Let's
 - denote Mario's points by M and Luigi's by L. Princess Peach is wondering how many postive
 - integers are there that are divisors to both numbers, M and L. Help her find the answer.
 -
 - Input:
 - First line of input contains an integer, T, which represent the number of test cases. Then
 - follows T lines. Each line contains two space separated integers, M L, representing the
 - points earned by Mario and Luigi, respectively.
 -}
mainCommonDivisors :: IO ()
mainCommonDivisors = do
  t <- readLn :: IO Int
  replicateM_ t (getLine >>= print . (\[x, y] -> cDivs x y) . map read . words)


-- SUBSET SUM -------------------------------------------------------------------------------------

{-|
 - Cumulative sum for a given list.
 -}
cumsum :: [Int] -> [Int]
cumsum = scanl1 (+)

{-|
 - Cumulative sum for a list whose elements are first arranged in descending order.
 -}
desCumSum :: [Int] -> [Int]
desCumSum = cumsum . sortBy (flip compare)

subsetSum :: [Int] -> [Int] -> [Int]
subsetSum as ts =
  let
    -- | sorts the testcases in ascending order (but zips then with the index so we can
    -- display the output as we recieved it
    sortedTests :: [Int] -> [(Int, Int)]
    sortedTests = sortBy (comparing snd) . zip [1..]

    -- | Couldn't find a better name
    -- takes indexed descending cummulative sums and indexed testcase inputs
    -- and associates the the descending cum-sum with the index of the test case
    -- Basic Idea:
    -- If testcases end return an empty list
    -- If we exhaust the list => all other test cases are greater than sum of list so associate -1.
    -- otherwise in the general case
    -- if the current number < test case = delete the number and recurse
    -- otherwise = associate the index of list entry with the out put index
    -- (signifying how many elements are required to satisfy the test case)
    f :: (Ord a, Num b) => [(b, a)] -> [(t, a)] -> [(t, b)]
    f _ [] = []
    f [] ys = map (\(iy, _) -> (iy, -1)) ys
    f xs'@((ix, x):xs) ys'@((iy, y):ys)
        | x < y = f xs ys'
        | otherwise = (iy, ix) : f xs' ys
  in
    map snd . sort $ f (zip [1..] $ desCumSum as) (sortedTests ts)

{-|
 - You are given a list of N positive integers, A = {a[1], a[2], ..., a[N]} and another integer S.
 - You have to find whether there exists a non-empty subset of A whose sum is greater than or
 - equal to S.
 -
 - You have to print the size of minimal subset whose sum is greater than or equal to S. If there
 - exists no such subset then print -1 instead.
 -
 - Input:
 - First line will contain an integer, N, which is the size of list A. Second line contains N
 - space separated integers, representing the elements of list A. In third line there is an
 - integer, T, which represent the number of test cases to follow. Then follows T lines. Each one
 - of them contains an single integer, S.
 -
 - Output:
 - For each test case, print the size of minimal subset whose sum is greater than or equal to S.
 - If there's no such subset then print -1.
 -}
mainSubSum :: IO ()
mainSubSum = do
  as <- fmap (map (read :: String -> Int) . words) (getLine >> getLine)
  ts <- fmap (map (read :: String -> Int) . lines) (getLine >> getContents)
  putStr . unlines . map show $ subsetSum as ts


-- CAPTIAN PRIME ----------------------------------------------------------------------------------

{-| Set of all the prime numbers less than a million -}
primeSet :: S.Set Int
primeSet =
  S.fromList (takeWhile (< (10^(6 :: Int))) primes)

{-|
 - Based on the criteria (mentioned in desc of mainCaptianPrime) give roles to various shipmates.
 -}
whereYouGo :: S.Set Int -> Int -> String
whereYouGo ps n
  | hasZero n = "DEAD"
  | l && r = "CENTRAL"
  | l = "RIGHT"
  | r = "LEFT"
  | otherwise = "DEAD"
    where
      l = all (== True) (map (`S.member` ps) (fromLeft n))
      r = all (== True) (map (`S.member` ps) (fromRight n))


{-|
 - Captain Prime is going on a trip to Primeland and needs support of his troops to make this
 - voyage successful. To prevent the wrath of evil spirits, he has to throw out some people from
 - his troop into the sea. This decision will depend on the identification number of the troop
 - member. 
 - 
 - His ship is divided into three parts: Left, right, and central. Every person on the ship is
 - assigned an identification number (referred as id), and according to their id they get to work
 - in one part of the ship, or end up getting thrown out of the ship. 
 - 
 - 
 - A person's fate depends on the following conditions:
 - 
 - CENTRAL: He will be working in central part if (a) his id is a prime number, (b) it doesn't
 - contain 0 as one of the digits, and (c) when the left digits are successively taken off, then
 - all the resulting numbers are also prime. (d) And same goes for the digits on the right side.
 - For example person with id 3137 will work in central part, as 3137, {313, 31, 3}, {137, 37, and
 - 7} are all prime numbers.
 - 
 - LEFT: He will be working in left part if (a) his id is a prime number, (b) and doesn't contain
 - 0 as one of the digits. (c) Also when the left digits are successively taken off, then all the
 - resulting numbers are prime, but this doesn't hold true for the right digits. For example,
 - person with id 1367 will work here, since 1367, 367, 67 and 7 are all prime numbers. While 136
 - is not a prime number, which we get after removing one digit on the right.
 - 
 - RIGHT: He will be working on right part if (a) his id is a prime number, (b) and doesn't contain
 - 0 digit as one of the digits. (c) Also on successively stripping right digits, all the resulting
 - numbers are prime, but this does not hold true for the left digits. For example, person with id
 - 2333 belongs to this category, as 2333, 233, 23 and 2 are all prime numbers, while 333 is not a
 - prime number.
 - 
 - DEAD: If a person is not eligible to work anywhere on the ship, then he will be thrown out of
 - the ship. Sad!
 - 
 - Input Format: 
 - The first line contains T, the number of persons on the ship, followed by the their id numbers
 - in the next T lines.
 - 
 - Output Format: 
 - Print LEFT, RIGHT, CENTRAL, or DEAD according to the fate of the person on the ship.
 - 
 - Constraints: 
 - 1<= T <= 50
 - 1 <= id <= 10^6
 -}
mainCaptianPrime :: IO ()
mainCaptianPrime =
  getLine >> getContents >>= putStr . unlines . map (whereYouGo primeSet . read) . lines


-- MAIN -------------------------------------------------------------------------------------------

adhoc :: [(String, IO ())]
adhoc =
  [ ("jumpingBunnies", mainJumpingBunnies)
  , ("rotations", mainRotations)
  , ("reduction", mainReduction)
  , ("hugeGCD", mainGCD)
  , ("bubbleWrap", mainBubbleWrap)
  , ("missingNums", mainMissingNums)
  , ("commonDivisors", mainCommonDivisors)
  , ("subsetSum", mainSubSum)
  , ("captianPrime", mainCaptianPrime)
  ]

