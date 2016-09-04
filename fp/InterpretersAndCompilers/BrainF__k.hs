{-# OPTIONS_GHC -Wall #-}

import Control.Applicative ((<$>))
import Data.Char (chr, ord)
import Data.List (intercalate)
import Text.ParserCombinators.Parsec
  (
    Parser
  , char
  , many
  , parse
  , (<|>)
  )

-- TOKENIZER --

{-|
 - Remove everything but the valid BrainF__k tokens
 -}
stripComments :: String -> String
stripComments =
  filter (`elem` "><+-.,[]")


{-
 - > : Increment data pointer so that it points to next location in memory.
 -
 - < : Decrement data pointer so that it points to previous locaion in memory.
 -
 - + : Increment the byte pointed by data pointer by 1. If it is already at its maximum value,
 -     255, then new value will be 0.
 -
 - - : Decrement the byte pointed by data pointer by 1. If it is at its minimum value, 0, then new
 -     value will be 255.
 -
 - . : Output the character represented by the byte at the data pointer.
 -
 - , : Read one byte and store it at the memory location pointed by data pointer.
 -
 - [ : If the byte pointed by data pointer is zero, then move instruction pointer to next matching
 -     ']', otherwise move instruction pointer to next command.
 -
 - ] : If the byte pointed by data pointer is non-zero, then move instruction pointer to previous
 -     matching '[' command, otherwise to next command.
 -}
data Token =
    TRight
  | TLeft
  | TInc
  | TDec
  | TPrint
  | TRead
  | TLoopS
  | TLoopE
  | End
  deriving (Show)


{-|
 - Parse a single BrainF__k token
 -}
token :: Parser Token
token =
      const TRight <$> char '>'
  <|> const TLeft  <$> char '<'
  <|> const TInc   <$> char '+'
  <|> const TDec   <$> char '-'
  <|> const TPrint <$> char '.'
  <|> const TRead  <$> char ','
  <|> const TLoopS <$> char '['
  <|> const TLoopE <$> char ']'


{-|
 - Parse striped BrainF__k source code
 -}
tokens :: Parser [Token]
tokens =
  many token


{-|
 - Parse the BrainF__k source file
 -}
parseBF :: String -> [Token]
parseBF s =
  case parse tokens "" (stripComments s) of
    Left e ->
      error (show e)

    Right s' ->
      s'


-- The Infinite Turing Machine Tape --

{-|
 - This data type represents the infinite pt in the turing machine.
 - [Left of the pivot element] {Pivot element} [Right of the pivot element]
 -}
data Tape a =
  Tape [a] a [a]

instance Show a => Show (Tape a) where
  show (Tape ls p rs) =
    let
      showleft = "... ," ++ intercalate "," (reverse . map show $ take 10 ls) ++ "]"
      showriht = "[" ++ intercalate "," (map show $ take 10 rs) ++ ", ..."
      showpivt = "{" ++ show p ++ "}"
    in
      showleft ++ showpivt ++ showriht


fromList :: [a] -> Tape a
fromList [] = error "empty List"
fromList (x:xs) =
  Tape [] x xs


{-|
 - An Infinite tape of 0's
 -}
empty :: Tape Int
empty =
  Tape (repeat 0) 0 (repeat 0)

{-|
 - Move left on the tape
 -}
moveLeft :: Tape a -> Tape a
moveLeft (Tape [] _ _) = error "Fell of the left end!! Shouldn't be possible"
moveLeft (Tape (l:ls) p rs) =
  Tape ls l (p:rs)


{-|
 - Move right on the tape
 -}
moveRight :: Tape a -> Tape a
moveRight (Tape _ _ []) = error "Fell of the right end!! Shouldn't be possible"
moveRight (Tape ls p (r:rs)) =
  Tape (p:ls) r rs

{-|
 - Increment the focused cell by one
 -}
incPivot :: Tape Int -> Tape Int
incPivot (Tape ls p rs) =
  Tape ls ((p + 1) `mod` 256) rs


{-|
 - Decrement the focused cell by one
 -}
decPivot :: Tape Int -> Tape Int
decPivot (Tape ls p rs) =
  Tape ls ((p - 1) `mod` 256) rs

{-|
 - Retrive the value at the focused cell
 -}
getPivot :: Tape a -> a
getPivot (Tape _ p _) =
  p


{-|
 - Set a value to the focused cell
 -}
setPivot :: a -> Tape a -> Tape a
setPivot val (Tape ls _ rs) =
  Tape ls val rs


eval :: Int -> String -> Tape Token -> Tape Int -> String
eval _ ['$'] (Tape _ TRead _) _ = error "Calling read on end of input"
eval _ [] (Tape _ TRead _) _    = error "Wow Just Wow"
eval t ins it pt =
  if t > 100000
    then "\nPROCESS TIME OUT. KILLED!!!"
    else
      case getPivot it of
        End    -> []
        TRight -> eval (t + 1) ins (moveRight it) (moveRight pt)
        TLeft  -> eval (t + 1) ins (moveRight it) (moveLeft pt)
        TInc   -> eval (t + 1) ins (moveRight it) (incPivot pt)
        TDec   -> eval (t + 1) ins (moveRight it) (decPivot pt)
        TPrint -> chr (getPivot pt) : eval (t + 1) ins (moveRight it) pt
        TRead  -> eval (t + 1) (tail ins) (moveRight it) (setPivot (ord $ head ins) pt)
        TLoopS ->
          case getPivot pt of
            0 -> eval (t + 2) ins (moveRight it') pt where it' = gotoClose (-1) it
            _ -> eval (t + 1) ins (moveRight it) pt
        TLoopE ->
          case getPivot pt of
            0 -> eval (t + 1) ins (moveRight it) pt
            _ -> eval (t + 2) ins (moveRight it') pt where it' = gotoOpen (-1) it


{-| Goto the matching closing paren -}
gotoClose :: Int -> Tape Token -> Tape Token
gotoClose nl it =
  case getPivot it of
    TLoopE -> if nl == 0 then it else gotoClose (nl - 1) (moveRight it)
    TLoopS -> gotoClose (nl + 1) (moveRight it)
    _      -> gotoClose nl (moveRight it)


{-| Goto the matching closing paren -}
gotoOpen :: Int -> Tape Token -> Tape Token
gotoOpen nl it =
  case getPivot it of
    TLoopS -> if nl == 0 then it else gotoOpen (nl - 1) (moveLeft it)
    TLoopE -> gotoOpen (nl + 1) (moveLeft it)
    _      -> gotoOpen nl (moveLeft it)


main :: IO ()
main = do
  input <- getLine >> getLine
  program <- getContents
  putStr $ eval 0 input (fromList $ parseBF program ++ [End]) empty
