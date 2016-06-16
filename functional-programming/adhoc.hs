import qualified Data.Set as Set

---------------------------------------------------------------------------------------------------

{-|
 - Generate all iterations of a given list.
 -}
rotations :: [a] -> [[a]]
rotations xs = [ rotate i xs | i <- [1..(length xs)] ]
    where rotate n ys = take (length ys) (drop n (cycle ys))
---------------------------------------------------------------------------------------------------

{-|
 - Taks a list of orderable items and returns a new list of uniques preserving the first occurences
 - order.
 -}
reduction :: (Ord a) => Set.Set a -> [a] -> [a]
reduction seen []     = []
reduction seen (x:xs) =
  if Set.member x seen
  then (reduction seen xs)
  else x:(reduction (Set.insert x seen) xs)

---------------------------------------------------------------------------------------------------

