{-
 - Basic warup challanges of warmup category of Algorithms class on hacker rank
 -}

import qualified Data.Time as Time
---------------------------------------------------------------------------------------------------

{-| Add two numbers -}
addTwo :: (Num a) => a -> a -> a
addTwo a b = a + b
---------------------------------------------------------------------------------------------------

{-| Sum up all the elements of an array -}
arrSum :: (Num a) => [a] -> a
arrSum = sum
---------------------------------------------------------------------------------------------------

{-| Differance of the sum of diagonals of a matrix -}
diagonalDiff :: (Num a) => [[a]] -> a
diagonalDiff xs = abs (sum (primary xs) - sum (secondary xs))
  where
    -- the primary diagonal
    primary []     = []
    primary (x:xs) = head x : primary (map tail xs)
    -- the secondary diagonal
    secondary      = primary . map reverse
---------------------------------------------------------------------------------------------------

{-| The fractionalLen of negatives, zeros and positive quntities in a given list -}
fractionalLen :: (Ord a, Num a, Fractional b) => [a] -> [b]
fractionalLen xs = map (\x -> len (filter x xs) / len xs) [(< 0), (== 0), (> 0)]
  where len = fromIntegral . length
---------------------------------------------------------------------------------------------------
{-| 
 - outputs a staircase of size n
 - ["####", " ###", "  ##", "   #"] for n = 4
 -}
staircase :: Int -> [String]
staircase 0 = []
staircase n = concat (replicate n "#") : map (' ' : ) (staircase (n - 1))
---------------------------------------------------------------------------------------------------
{-|
 - Given a time in AM/PM format, converts it to military (24-hour) time.
 -}
ampmToHrs :: String -> String
ampmToHrs = wTime . rTime
  where
    wTime   = Time.formatTime Time.defaultTimeLocale "%H:%M:%S"
    rTime x = Time.parseTimeOrError True Time.defaultTimeLocale "%I:%M:%S%p" x :: Time.UTCTime
---------------------------------------------------------------------------------------------------

