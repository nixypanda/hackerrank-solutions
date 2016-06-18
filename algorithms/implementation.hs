{-
 - Solutions to implementation challanges in algorithms track on hacker-rank.
 -}

---------------------------------------------------------------------------------------------------

{-|
 - Finds out if the professor gets angry enough to cancel the class?
 - He gets angry if less than k student reach on time. <=0 => before/on Time
 -}
isProfAngry :: (Ord a, Num a) => [a] -> Int -> [Char]
isProfAngry arr k = if length (filter (<= 0) arr) < k then "YES" else "NO"
---------------------------------------------------------------------------------------------------

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
---------------------------------------------------------------------------------------------------

{-|
 - Calculates the length of the utopian tree at a given snapshot in time
 - The Utopian Tree goes through 2 cycles of growth every year. Each spring, it doubles in height.
 - ach summer, its height increases by 1 meter.
 -}
utopHeight :: (Integral a, Integral b) => a -> b
utopHeight 0 = 1
utopHeight n = if n `mod` 2 /= 0 then 2 * (utopHeight (n - 1)) else (utopHeight (n - 1)) + 1
---------------------------------------------------------------------------------------------------

{-| Digits that make up n and also evenly divide it.  -}
evenDivisors num = reverse (filter (\x -> x /= 0 && num `mod` x == 0) (digs num))
  where
    digs 0 = []
    digs n = (n `mod` 10) : digs (n `div` 10)
---------------------------------------------------------------------------------------------------

{-| Number of squares in a given limit -}
squares lo hi = floor (hi**(1.0/2.0)) - ceiling (lo**(1.0/2.0)) + 1
---------------------------------------------------------------------------------------------------

{-|
 - Which vechicle can I squeeze through the service lane
 - width : the width of all the service lane points
 - start to end the points between which we check vechicale we can squeeze.
 -}
vechile width start end = minimum (take (end - start + 1) (drop start width))
---------------------------------------------------------------------------------------------------

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
  where cutof = filter (\x -> x /= 0) [x - (minimum array) | x <- array]
---------------------------------------------------------------------------------------------------

{-|
 - Little Bob loves chocolate, and he goes to a store with $N in his pocket. The price of each
 - chocolate is $C. The store offers a discount: for every  wrappers M he gives to the store,
 - he gets one chocolate for free. How many chocolates does Bob get to eat?
 -}
-- get the initial number of choclates and then get how many
-- one can by using wrappers
choclates n c m = canBuy + (fromwraps canBuy m)
  where
    canBuy = n `div` c
    -- recursivly get choclates until can't get anymore
    fromwraps ws mws
      | ws < mws = 0
      | ws == mws = 1
      -- wrappers / (minimum wrappers) to get choclates
      -- then add the new wrappers to the remaining ones and ask for choclates again
      | otherwise = chocs + (fromwraps ((ws `mod` mws) + chocs) mws) where chocs = ws `div` mws
---------------------------------------------------------------------------------------------------

