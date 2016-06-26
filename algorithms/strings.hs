{-|
 - Module containg solutions to hacker-rank algorithms/strings sub module.
 -}

-- Library imports
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.List as L


-- PANGRAMS ---------------------------------------------------------------------------------------

{-|
 - Roy wanted to increase his typing speed for programming contests. So, his friend advised him to
 - type the sentence "The quick brown fox jumps over the lazy dog" repeatedly, because it is a
 - pangram. (Pangrams are sentences constructed by using every letter of the alphabet at least once)
 -
 - After typing the sentence several times, Roy became bored with it so he started to look for other
 - pangrams.
 -
 - Given a sentence , tell Roy if it is a pangram or not.
 -}
-- convert all string to lowercase | make a set | length of that set should be 27 
-- (' ' and 26 english alphabets)
isPangram :: String -> Bool
isPangram x = 27 == S.size (S.fromList $ map C.toLower x)


-- ALTERNATING CHARACTERS -------------------------------------------------------------------------

{-|
 - Shashank likes strings in which consecutive characters are different. For example, he likes
 - ABABA, while he doesn't like ABAA. Given a string containing characters  and  only, he wants
 - to change it into a string he likes. To do this, he is allowed to delete the characters in the
 - string.
 -
 - Your task is to find the minimum number of required deletions.
 -}
-- track the previous unique letter and incase it's equal to the new char increment the
-- count and continue otherwise replace it with the new one without incrementing the count
-- and continue.
makeLikeable :: (Integral a) => Char -> String -> a
makeLikeable _ []     = 0
makeLikeable p (x:xs) = if p == x then 1 + makeLikeable p xs else makeLikeable x xs


-- THE LOVE-LETTER MYSTRY -------------------------------------------------------------------------

{-|
 - Find the minimum number of operations required to convert a given string into a palindrome.
 -}
-- take the ascii values of all the characters in a string
-- zip it with its reverse and take the absolute values effectivly giving the distance
-- between the two chars
-- take half of them and sum them
distanceToPal :: String -> Int
distanceToPal xs = sum . take (length xs `div` 2) $ map abs $ zipWith (-) ys (reverse ys)
  where ys = map C.ord xs


-- GEMSTONES --------------------------------------------------------------------------------------

{-|
 - John has discovered various rocks. Each rock is composed of various elements, and each element
 - is represented by a lower-case Latin letter from 'a' to 'z'. An element can be present multiple
 - times in a rock. An element is called a gem-element if it occurs at least once in each of the
 - rocks.
 - 
 - Given the list of N rocks with their compositions, display the number of gem-elements that exist
 - in those rocks.
 -}
-- given a list of strings create a set from each one
-- find the intersection of the sets (giving the common elements in them)
-- its size is the required value.
gemstones :: [String] -> Int
gemstones xs = S.size . foldl1 S.intersection $ map S.fromList xs 


-- FUNNY STRINGS ----------------------------------------------------------------------------------

{-|
 - Consider a string S and it's reverse R then it's funny if |S_i - S_{i+1}| = |R_i - R_{i+1}|
 - for all i. 
 -}
-- calculate the difference list (stringDiff) of string and it's reverse
-- if they are equal => string is funny.
isFunny :: String -> Bool
isFunny xs = stringDiff xs == stringDiff (reverse xs)
  -- "abcd" -> [ 'a' - 'b', 'b' - 'c', 'c' - 'd' ]
  where stringDiff lst = zipWith (\x y -> abs $ C.ord x - C.ord y) lst (tail lst)


-- ANAGRAM ----------------------------------------------------------------------------------------

{-|
 - Given two strings S1 and S2 which are concataneted into one such that ||S1| - |S2|| < 1
 - find how many minimal changes are required such that an anagram can be formed out of them
 -}
-- NOTE: xs = string one ++ string two
-- create map of occurances of characters in first and second string (ys and zs)
-- find the difference between the occurances of chars from first to second
-- sum up that difference.
anagram :: (Integral a) => String -> a
anagram xs = sum . M.elems $ M.differenceWith sub ys zs
  where
    mid       = length xs `div` 2
    ys        = M.fromListWith (+) [(y, 1) | y <- take mid xs]
    zs        = M.fromListWith (+) [(z, 1) | z <- drop mid xs]
    sub vl vr = Just (max 0 $ vl - vr)


-- PALINDROME INDEX -------------------------------------------------------------------------------

{-|
 - Find the index of the number post whose removal the remaining string will become a palindrome
 -}
-- start from either end of the string checking if characters are equal
-- then in case of a mismatch drop either of the characters and check if the remaining
-- string is equal. (Now as the problem guarentees that their will be a valid answer we
-- can get away by checking only one case)
palindromeCheck zs = palCheck 0 (length zs - 1) zs (reverse zs)
  where
    isPal xs             = xs == reverse xs
    palCheck _ _ [x] [y] = -1
    palCheck xi yi (x:xs) rs@(y:ys)
      | x == y    = palCheck (xi + 1) (yi - 1) xs ys
      -- take (yi - xi - 1) (y:ys) gives the remaining string
      | otherwise = if isPal (take (yi - xi - 1) rs) then xi else yi


-- TWO STRINGS ------------------------------------------------------------------------------------

{-|
 - Given two strings find if there exists a common substring in both of them
 -}
-- simply find the intersection and see if it's greater than 0
hasCommonSubstring :: String -> String -> Bool 
hasCommonSubstring xs ys = S.size (S.intersection (S.fromList xs) (S.fromList ys)) > 0


-- GAME OF THRONES I ------------------------------------------------------------------------------

{-| 
 - The king has a string composed of lowercase English letters. Help him figure out whether any
 - anagram of the string can be a palindrome or not.
 -}
-- frequenciey map of occurances of each char
-- if length is odd then only one should have odd occurances rest should have even
-- incase of even length all should have an even number of occurances
isPalable :: String -> Bool 
isPalable s
  | length s `mod` 2 == 0 = all (== True) elmap
  | otherwise             = length (filter (== False) elmap) == 1
  where elmap = map (\x -> x `mod` 2 == 0) . M.elems $ M.fromListWith (+) [(x, 1) | x <- s]


-- MAKING ANAGRAMS --------------------------------------------------------------------------------

{-|
 - Given two strings (they can be of same or different length) help her in finding out the minimum
 - number of character deletions required to make two strings anagrams. Any characters can be
 - deleted from any of the strings.
 -}
makeAnagram :: (Integral a) => String -> String -> a
makeAnagram xs ys = sum $ concatMap M.elems [M.differenceWith sub fx fy, M.differenceWith sub fy fx]
  where
    fx      = M.fromListWith (+) [(x, 1) | x <- xs]
    fy      = M.fromListWith (+) [(y, 1) | y <- ys]
    sub x y = Just (max 0 $ x - y)


-- BEAR AND STEADY GENE ---------------------------------------------------------------------------

{-|
 - A gene is represented as a string of length n (where  is divisible by 4), composed of the
 - letters A, C, G, and T. It is considered to be steady if each of the four letters occurs exactly  - times. For example, GACT and AAGTGCCT are both steady genes.
 -
 - Bear Limak is a famous biotechnology scientist who specializes in modifying bear DNA to make it
 - steady. Right now, he is examining a gene represented as a string s. It is not necessarily
 - steady. Fortunately, Limak can choose one (maybe empty) substring of s and replace it with
 - any substring of the same length.
 -
 - Modifying a large substring of bear genes can be dangerous. Given a string s, can you help Limak
 - find the length of the smallest possible substring that he can replace to make s a steady gene?
 -
 - Note: A substring of a string s is a subsequence made up of zero or more consecutive characters
 - of s.
 -}
-- TODO

-- SHERLOCK AND ANAGRAMS --------------------------------------------------------------------------

{-|
 - Given a string S, find the number of "unordered anagrammatic pairs" of substrings.
 -}
-- generate all the substrings.
-- sort each of the genrated substring.
-- create a frequency map from 2.
-- get all the values from 3 and filter out the ones with frequency greater than one.
-- now just calculte n choose 2 for each value in the list.
unorderedAns :: (Ord a, Integral c) => [a] -> c
unorderedAns xs = sum . map nc2 . M.elems $ M.fromListWith (+) [(x, 1) | x <- subs]
  where
    subs  = map L.sort $ (filter (not . null) . concatMap L.inits . L.tails) xs 
    nc2 n = n * (n - 1) `div` 2


-- SHERLOCK AND VALID STRING ----------------------------------------------------------------------

{-|
 - A "valid" string S is a string such that for all distinct characters in S each such character
 - occurs the same number of times in S.
 -}
isValidString :: String -> Bool
isValidString s = length tmap == 1 || (length tmap == 2 && elem 1 tmap)
  where 
    -- group together the frequencies
    tmap = M.elems $ M.fromListWith (+) [(e, 1) | e <- vals]
    -- group together the frequenciey of occurance of each elements
    vals = M.elems $ M.fromListWith (+) [(x, 1) | x <- s]


