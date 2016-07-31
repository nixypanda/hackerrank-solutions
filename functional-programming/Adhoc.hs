{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Adhoc
  ( adhoc
  )
  where

import qualified Data.Set as Set


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


mainReduction :: IO ()
mainReduction =
  getLine >>= putStrLn . reduction Set.empty


---------------------------------------------------------------------------------------------------

adhoc :: [(String, IO ())]
adhoc =
  [ ("rotations", mainRotations)
  , ("reduction", mainReduction)
  ]

