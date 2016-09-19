{-# OPTIONS_GHC -Wall #-}


import Control.Applicative
  (
    pure
  , (<$>)
  , (*>)
  , (<*)
  , (<*>)
  )
import Data.Functor.Identity (Identity)
import Data.Ratio
  (
    Rational
  , numerator
  , denominator
  , (%)
  )
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Text.ParserCombinators.Parsec
  (
    Parser 
  , alphaNum
  , char
--  , digit
  , letter
  , many
  , parse
  , sepBy1
  , try
--  , upper
  , (<|>)
  )
import Text.Parsec.Expr
  (
    Operator(Infix, Prefix)
  , Assoc(AssocLeft)
  , OperatorTable
  , buildExpressionParser
  )
import Text.ParserCombinators.Parsec.Language
  (
    LanguageDef
  , caseSensitive
  , commentStart
  , commentEnd
  , commentLine
  , emptyDef
  , identStart
  , identLetter
  , nestedComments
  , reservedNames
  , reservedOpNames
  )

import qualified Data.Map as M
import qualified Text.Parsec.Token as Token


---------------------------------------------------------------------------------------------------
-- TYPES DECLARATIONS -----------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

data Oper =
    Plus
  | Minus
  | Times
  | Divide
  deriving (Show, Eq)


data Expr =
    Var String [Expr]
  | Val Rational
  | Neg Expr
  | Op Oper Expr Expr
  deriving (Show, Eq)


data Statement =
    Assign String Expr
  | Sequence [Statement]
  | Declare String Integer [Expr]
  | What [Expr]
  | Do Expr Statement
  deriving (Show, Eq)


---------------------------------------------------------------------------------------------------
-- LEXICAL ANALYSIS -------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- Lexical token spec
intutiDef :: LanguageDef st
intutiDef =
  emptyDef
    { commentStart   = ""
    , commentEnd     = ""
    , commentLine    = ""
    , nestedComments = False
    , identStart     = letter
    , identLetter    = alphaNum
    , reservedNames  =
      [ "is"
      , "of"
      , "assign"
      , "what"
      , "function"
      , "do"
      , "and"
      , "to"
      ]
    , reservedOpNames =
      [ "*"
      , "/"
      , "+"
      , "-"
      , ":"
      ]
    , caseSensitive  = False
    }


-- Creating a lexer
lexer :: Token.TokenParser a
lexer =
  Token.makeTokenParser intutiDef 


-- Parse various peices of code (Just to simplify code piece that follow)
-- tip from haskell wiki.

-- parses an identifier
identifier :: Parser String
identifier =
  Token.identifier lexer

-- parses a reserved name
reserved :: String -> Parser ()
reserved =
  Token.reserved lexer

-- parses an operator
reservedOp :: String -> Parser ()
reservedOp =
  Token.reservedOp lexer

-- parses surrounding parenthesis: parens p takes care of the parenthesis and
-- uses p to parse what's inside them
parens :: Parser a -> Parser a
parens =
  Token.parens lexer

-- parses {}
braces :: Parser a -> Parser a
braces =
  Token.braces lexer

-- parses []
brackets :: Parser a -> Parser a
brackets =
  Token.brackets lexer

-- parses an integer
integer :: Parser Integer
integer =
  Token.integer lexer

-- parses a semicolon
semi :: Parser String
semi =
  Token.semi lexer

-- parses whitespace
whiteSpace :: Parser ()
whiteSpace =
  Token.whiteSpace lexer

-- parses a comma (,)
comma :: Parser String
comma =
  Token.comma lexer

-- parses a full-stop / period / (.)
dot :: Parser String
dot =
  Token.dot lexer


---------------------------------------------------------------------------------------------------
-- PARSING ----------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- EXPRESSIONS ------------------------------------------------------------------------------------


-- Arithmetic expressions --

-- buildExpressionParser does everything. It just needs what a term is and an OperatorTable.
expr :: Parser Expr
expr =
  buildExpressionParser arithOp term


-- parses function call (Inc[i])
functionCall :: Parser Expr
functionCall =
  Var <$> identifier <*> many (brackets expr) <* many (char ' ')


-- specifying what is a term in the arithmetic expression
term :: Parser Expr
term =
      parens expr
  <|> Val . (% 1) <$> integer
  <|> functionCall


-- specifying operators precedence, loc (pre, in or post) and associativity
arithOp :: OperatorTable String () Identity Expr
arithOp =
  [ [ Prefix (reservedOp "-" *> pure Neg)
    ]
  , [ Infix (reservedOp "*" *> pure (Op Times )) AssocLeft
    , Infix (reservedOp "/" *> pure (Op Divide)) AssocLeft
    ]
  , [ Infix (reservedOp "+" *> pure (Op Plus  )) AssocLeft
    , Infix (reservedOp "-" *> pure (Op Minus )) AssocLeft
    ]
  ]


-- STATEMENTS -------------------------------------------------------------------------------------

-- %Variable% is %Expression%. (Function with no parameters)
declareV :: Parser Statement
declareV = do
  var <- identifier <* many (char ' ') <* reserved "is"
  arg <- sepBy1 expr comma <* dot
  return $ Declare var 0 arg

{-|
 - Function is an expression of n parameters.
 - Function1 is function of n: k1, k2, ..., kn, k0.
 - represents a function called Function1, when called with n parameters it will yield following
 - value
 - k1*%1 + k2*%2 + ... + kn*%n + k0 
 - where %i is the ith parameter, and k0, k1, ..., kn are the coefficients. 
 - Formally, a function with n parameters is defined as 
 - %Variable% is function of %N%: %Expression1%, %Expression2%, ..., %ExpressionN%, %Expression0%.
 -  where k0 = Expression0, k1 = Expression1, ..., kn = Expressionn. 
 -}
declareF :: Parser Statement
declareF = do
  var <- identifier <* many (char ' ')
  n <- reserved "is" *> reserved "function" *> reserved "of" *> integer
  args <- reserved ":" *> sepBy1 expr comma <* dot
  return $ Declare var n args


-- | Covers all kinds of declarations
declaration :: Parser Statement
declaration =
  try declareV <|> try declareF


{-|
 - How to Assign a value to a variable?
 -
 - Assign %Expression1% to %Variable1% [ AND %Expression2% to %Variable2% ... ]!
 - 
 - This means that the value of Variablei will be Expressioni. 
 - Note: 
 -  1. Multiple assignments go left-to-right. 
 -  2. Assignment ends with an exclamation sign, '!'. 
 -  3. You can assign values to a variable multiple times. 
 -}
assigns :: Parser Statement
assigns =
  let
    assignment =
      flip Assign <$> expr <*> (reserved "to" *> identifier)
  in
    Sequence <$> (reserved "assign" *> sepBy1 assignment (reserved "and") <* char '!')

{-|
 - How to make a loop?
 - 
 - do {Expression} %Assign values(s) to variable(s)%!
 - 
 - Assignment inside the loop will be repeated number of times, indicated in {}. This number is
 - guaranteed to be positive integer. 
 -}
doAssign :: Parser Statement
doAssign =
  Do <$> (reserved "do" *> braces expr) <*> assigns


{-|
 - How to ask about variables and function's value?
 - 
 - what is %Function1 call% [AND %Variable1% [ AND %Function2 call% ... ] ]?
 - 
 - A function's value or variable's value can be asked anywhere, once it has been initialised.
 - Function call: %Name%[Expression1][Expression2]...[ExpressionM]. 
 - Expressioni will be the ith parameter, %i, in function declaration.
 - Results
 - If all n parameters are provided: the result of function call will be a value.
 - Otherwise: it will result into another function which takes remaining parameters. You have
 - print coefficients, comma and space separated, as output in a line.
 - Question ends with '?'.
 -}
whats :: Parser Statement
whats =
  What <$> (reserved "what" *> reserved "is" *> sepBy1 functionCall (reserved "and") <* char '?')


intutiveLang :: Parser Statement
intutiveLang =
  let
    statement =
          declaration
      <|> assigns
      <|> doAssign
      <|> whats
  in
    Sequence <$> (whiteSpace *> sepBy1 statement (char '\n'))


---------------------------------------------------------------------------------------------------
-- INTERPRETING LINE BY LINE ----------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

type State =
  M.Map String [Rational]


operate :: Rational -> [Rational] -> [Rational] -> [Rational]
operate val [] [] = [val]
operate _ _ [] = error "Invalid number of args"
operate val [] ys = reverse (val + head ys' : tail ys') where ys' = reverse ys
operate val (x:xs) (y:ys) = operate (val + x * y) xs ys


evalE :: State -> Expr -> [Rational]
evalE st (Var s []) = fromJust $ M.lookup s st
evalE st (Var s xs) = operate 0 (concatMap (evalE st) xs) (fromJust $ M.lookup s st)
evalE _  (Val v) = [v]
evalE st (Neg e) = [-1 * e'] where [e'] = evalE st e
evalE st (Op op e1 e2) =
  let
    [e1'] = evalE st e1
    [e2'] = evalE st e2
  in
    case op of
      Plus   -> [e1' + e2']
      Minus  -> [e1' - e2']
      Times  -> [e1' * e2']
      Divide -> [e1' / e2']


eval :: (State, [[Rational]]) -> Statement -> (State, [[Rational]])
eval (st, xs) (Assign var e) = (M.insert var (evalE st e) st, xs)
eval (st, xs) (Declare var _ ex) = (M.insert var (concatMap (evalE st) ex) st, xs)
eval (st, xs) (What ys) = (st, reverse (map (evalE st) ys) ++ xs)
eval (st, xs) (Sequence []) = (st, xs)
eval (st, xs) (Sequence (y:ys)) = eval (nst, xs') (Sequence ys)
  where (nst, xs') = eval (st, xs) y
eval (st, xs) (Do (Val 1) s) = eval (st, xs) s
eval (st, xs) (Do n s) =
  eval (nst, xs') s
    where
      [n'] = evalE st n
      (nst, xs') = eval (st, xs) (Do (Val (n' - toRational (1 :: Int))) s)


preetyR :: Rational -> String
preetyR r =
  case denominator r of
    1 ->
      show $ numerator r

    _ ->
      show (numerator r) ++ "/" ++ show (denominator r)

---------------------------------------------------------------------------------------------------
-- MAIN RUNNER ------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  input <- getContents
  case parse intutiveLang "" input of
    Left err ->
      print err

    Right r ->
      putStr . unlines . map (intercalate ", " . map preetyR) . reverse . snd $ eval (M.empty, []) r

