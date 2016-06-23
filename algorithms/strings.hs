{-|
 - Module containg solutions to hacker-rank algorithms/strings sub module.
 -}

-- Library imports
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Char as C

-- PANGRAMS ---------------------------------------------------------------------------------------

{-|
 - Roy wanted to increase his typing speed for programming contests. So, his friend advised him to
 - type the sentence "The quick brown fox jumps over the lazy dog" repeatedly, because it is a
 - pangram. (Pangrams are sentences constructed by using every letter of the alphabet at least once.)
 -
 - After typing the sentence several times, Roy became bored with it. So he started to look for other
 - pangrams.
 -
 - Given a sentence , tell Roy if it is a pangram or not.
 -}
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
makeLikeable :: (Integral a) => Char -> String -> a
makeLikeable _ []     = 0
makeLikeable p (x:xs) = if p == x then 1 + makeLikeable p xs else makeLikeable x xs


-- THE LOVE-LETTER MYSTRY -------------------------------------------------------------------------

{-|
 - Find the minimum number of operations required to convert a given string into a palindrome.
 -}
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
gemstones :: [String] -> Int
gemstones xs = S.size . foldl1 S.intersection $ map S.fromList xs 


-- FUNNY STRINGS ----------------------------------------------------------------------------------

{-|
 - Consider a string S and it's reverse R then it's funny if |S_i - S_{i+1}| = |R_i - R_{i+1}|
 - for all i. 
 -}
isFunny :: String -> Bool
isFunny xs = stringDiff xs == stringDiff (reverse xs)
  where stringDiff lst = zipWith (\x y -> abs $ C.ord x - C.ord y) (init lst) (tail lst)


-- ANAGRAM ----------------------------------------------------------------------------------------

{-|
 - Given two strings S1 and S2 which are concataneted into one such that ||S1| - |S2|| < 1
 - find how many minimal changes are required such that an anagram can be formed out of them
 -}
anagram :: (Integral a) => String -> a
anagram xs = M.foldl' (+) 0 $ M.differenceWith sub (M.fromListWith (+) ys) (M.fromListWith (+) zs)
  where
    mid       = length xs `div` 2
    ys        = [(y, 1) | y <- take mid xs]
    zs        = [(z, 1) | z <- drop mid xs]
    sub vl vr = Just (max 0 $ vl - vr)


-- PALINDROME INDEX -------------------------------------------------------------------------------

{-|
 - Find the index of the number post whose removal the remaining string will become a palindrome
 -}


-- TWO STRINGS ------------------------------------------------------------------------------------

{-|
 - Given two strings find if there exists a common substring in both of them
 -}
hasCommonSubstring :: String -> String -> Bool 
hasCommonSubstring xs ys = (S.size $ S.intersection (S.fromList xs) (S.fromList ys)) > 0


-- GAME OF THRONES I ------------------------------------------------------------------------------

{-| 
 - The king has a string composed of lowercase English letters. Help him figure out whether any
 - anagram of the string can be a palindrome or not.
 -}
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
makeAnagram xs ys =
  (M.foldl' (+) 0 $ M.differenceWith sub fxs fys) + (M.foldl' (+) 0 $ M.differenceWith sub fys fxs)
  where
    fxs       = M.fromListWith (+) [(x, 1) | x <- xs]
    fys       = M.fromListWith (+) [(y, 1) | y <- ys]
    sub vl vr = Just (max 0 $ vl - vr)


---------------------------------------------------------------------------------------------------
