{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Adhoc
  ( adhoc
  , factors
  )
  where

import Control.Monad (replicateM_, forM_)
import Control.Monad.ST (ST)
import Data.List (foldl1)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Maybe as M
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV


-- HELPERS ---------------------------------------------------------------------------------------

factors :: Integral a => a -> [a]
factors n =
  let
    lows = filter ((== 0) . mod n) [1..truncate . (sqrt :: Double -> Double) $ fromIntegral n]
  in 
    lows ++ reverse (map (div n) lows)


-- HELPERS ---------------------------------------------------------------------------------------

{-|
 - Calculate the lcm of a given list of numbers.
 -}
lcm' :: [Int] -> Int
lcm' = foldl1 lcm

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
reduction :: (Ord a) => Set.Set a -> [a] -> [a]
reduction _ []     = []
reduction seen (x:xs) =
  if Set.member x seen
    then reduction seen xs
    else x : reduction (Set.insert x seen) xs

{-|
 - You are given a string, str, of length N consisting of lowercase letters of alphabet. You have
 - to remove all those characters from str which have already appeared in it, i.e., you have to
 - keep only first occurance of each letter.
 -}
mainReduction :: IO ()
mainReduction =
  getLine >>= putStrLn . reduction Set.empty


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
  Set.size . Set.fromList . factors $ gcd x y

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
  ]

