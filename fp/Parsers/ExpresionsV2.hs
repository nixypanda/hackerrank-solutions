{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

import Control.Applicative
  (
    (<*)
  , (<*>)
  , (<$>)
  )

import Text.ParserCombinators.Parsec
  (
    Parser
  , between
  , chainr1
  , char
  , digit
  , many
  , many1
  , oneOf
  , parse
  , string
  , (<|>)
  )

-- GENRAL HELPERS ---------------------------------------------------------------------------------

{-|
 - All the results will be mod the following number
 -}
modp :: Int
modp = 1000000007

{-|
 - Fast modular exponentiation
 -}
modpow :: Int -> Int -> Int -> Int
modpow _ 0 _ = 1
modpow b e m =
  modpow (b*b `mod` m) (e `div` 2) m * (if e `mod` 2 == 1 then b else 1) `mod` m


-- EXPRESSIONS V2 ---------------------------------------------------------------------------------

{-|
 - a uniary + or -
 -}
type UOp =
  Int -> Int

{-|
 - a binary +, =, *, / (for ints)
 -}
type BOp =
  Int -> Int -> Int

{-|
 - expr -> expr (*|/|+-) expr | (+|-) expr | number
 - number -> any whole number
 -}
data Expr =
    B BOp Expr Expr
  | U UOp Expr
  | N Int


-- OPERATOR CONVERSION --

toBOp :: Char -> Expr -> Expr -> Expr
toBOp '+' = B (+)
toBOp '-' = B (-)
toBOp '*' = B (*)
toBOp '/' = B (\a b -> a * modpow b (modp - 2) modp)
toBOp  _  = error "Nice operator but what do I do with it."

toUOp :: Char -> Expr -> Expr
toUOp '+' = U id
toUOp '-' = U negate
toUOp  _  = error "Nice operator but what do I do with it."


-- PARSE HELPERS --

{-|
 - Whitespace parser
 -}
ws :: Parser String
ws = many (char ' ')


-- PARSE OPERATORS --

{-|
 - Parses '+' or '-' into binary operators
 -}
addSub :: Parser (Expr -> Expr -> Expr)
addSub =
  toBOp <$> oneOf "+-" <* ws


{-|
 - Parses '*' or '/' into binary operators
 -}
mulDiv :: Parser (Expr -> Expr -> Expr)
mulDiv =
  toBOp <$> oneOf "*/" <* ws


{-|
 - Parses '+' or '-' into unary operators
 -}
unary :: Parser Expr
unary =
  toUOp <$> oneOf "+-" <*> factor


-- PARSE EXPRESSIONS, TERMS and FACTORS --

{-|
 - Number parser
 -}
num :: Parser Expr
num =
  N . read <$> many1 digit <* ws


{-|
 - Expressions between parens
 -}
parens :: Parser Expr
parens = between (symbol "(") (symbol ")") expression
  where
    symbol :: String -> Parser String
    symbol s = string s <* ws


{-|
 - Expression ::= Term [+-] Expression
 -              | Term
 -}
expression :: Parser Expr
expression =
  term `chainr1` addSub

{-|
 - Term ::= Factor [*/] Term
 -        | Factor
 -}
term :: Parser Expr
term =
  factor `chainr1` mulDiv 


{-|
 - Factor ::= Number
 -          | [+-] Factor
 -          | '(' Expression ')'
 -}
factor :: Parser Expr
factor =
      num
  <|> unary
  <|> parens

eval :: Expr -> Int
eval (N i) = i
eval (U u e) = u (eval e)
eval (B b e1 e2) = (eval e1 `b` eval e2) `mod` modp

{-|
 - Once Kazama had written a basic calculator, he read further about other operators and operator
 - precedence. Now he is writing a new calculator with following details:
 - 
 - Binary addition: (x + y).
 - Binary subtraction: (x - y) 
 - Multiplication: (x * y)
 - Division: (x / y)
 - Unary operators: +x and -x
 - Brackets: (...)
 - Operator precedence: (Unary, Brackets) > (Multiplication, Division) > (Binary add, Binary Sub)
 - 
 - 
 - Associativity:
 - Now all operators are right associative. That is x - y - z = z - (y - z)
 - 
 - Formally it has following grammar:
 - 
 -  Expression ::= Term [+-] Expression
 -               | Term
 - 
 -  Term       ::= Factor [*/] Term
 -               | Factor
 - 
 -  Factor     ::= Number
 -               | [+-] Factor
 -               | '(' Expression ')'
 - 
 - He needs your help to verify it. He wants you to solve some expressions for him using the above
 - grammar and he will cross check the results. Since you are also lazy, you will write another
 - computer program which will solve the expressions. Since the output value can be too large, you
 - have to tell output modulo 1000000007
 - 
 - Note:
 - 1000000007 is a prime.
 - 1/b = b^(-1) = b^(p - 2) (mod p), where p is prime and b < p
 - 
 - Input Format:
 - Input will contain a valid expression.
 - 
 - Constraints
 - Length of expression will not exceed 10000
 - There can be 0 or more whitespaces between operators/operands.
 - Tests are designed such that there will be no divide by zero case.
 - Each factor will be accompanied by at-most one unary operator.
 - 
 - Output Format
 - Print the result of expression modulo 1000000009
 -}
main :: IO ()
main = do
  input <- getContents
  case parse expression "" input of
    Left err -> print err
    Right expr -> print (eval expr)

