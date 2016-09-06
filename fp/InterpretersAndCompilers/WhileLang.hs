{-# OPTIONS_GHC -Wall #-}

import Control.Applicative
  (
    pure
  , (<$>)
  , (*>)
  )
import Data.Functor.Identity (Identity)
import Data.Maybe (fromJust)
import Text.Parsec
  (
    alphaNum
  , letter
  )
import Text.Parsec.Expr
  (
    Operator(Infix)
  , Assoc(AssocLeft)
  , OperatorTable
  , buildExpressionParser
  )
import qualified Text.Parsec.Token as Token
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
import Text.ParserCombinators.Parsec
  (
    Parser
  , parse
  , sepBy1
  , (<|>)
  )
import qualified Data.Map.Strict as M

---------------------------------------------------------------------------------------------------
-- TYPES DECLARATIONS -----------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

{-|
 - The Arithmetic operators.
 - opa ∈ Opa (arithmetic operators)
 - oba ::= + | - | * | /
 -}
data ArithOp =
    Plus
  | Minus
  | Times
  | Divide
  deriving (Show, Eq)

{-|
 - Grammer for arithmetic expressions
 - a ∈ ArithExpr (arithmetic expressions
 - a ::= x | n | a1 opa a2 | ( a )
 -}
data ArithExpr =
    AVar String
  | AVal Integer
  | AOp ArithOp ArithExpr ArithExpr
  deriving (Show, Eq)

{-
 - The Boolean operators.
 - opb ∈ Opb (boolean operators)
 - opb ::= and | or
 -}
data BoolOp =
    And
  | Or
  deriving (Show, Eq)

{-|
 - The Relational Operators
 - opr ∈ Opr (relational operators)
 - opr ::= > | <
 -}
data RelationalOp =
    Gt
  | Lt
  deriving (Show, Eq)


{-|
 - The Boolean Expressions
 - b ∈ BoolExpr (boolean expressions)
 - b ::= true | false | b1 opb b2 | a1 opr a2 | ( b )
 -}
data BoolExpr =
    BVal Bool
  | B BoolOp BoolExpr BoolExpr
  | A RelationalOp ArithExpr ArithExpr
  deriving (Show, Eq)


{-
 - The Statements of the while language.
 - S ∈ Stmt (statements)
 - S ::= x := a | S1 ; S2 | if b then { S1 } else { S2 } | while b do { S }
 -}
data Statement =
    Assign String ArithExpr
  | While BoolExpr Statement
  | Sequence [ Statement ]
  | If BoolExpr Statement Statement
  deriving (Show, Eq)


---------------------------------------------------------------------------------------------------
-- LEXICAL ANALYSIS -------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- Lexical token spec
whileDef:: LanguageDef st
whileDef =
  emptyDef
    { commentStart   = ""
    , commentEnd     = ""
    , commentLine    = ""
    , nestedComments = False
    , identStart     = letter
    , identLetter    = alphaNum
    , reservedNames  =
      [ "if"
      , "then"
      , "else"
      , "while"
      , "do"
      , "true"
      , "false"
      , "and"
      , "or"
      ]
    , reservedOpNames =
      [ "*"
      , "/"
      , "+"
      , "-"
      , "and"
      , "or"
      , ">"
      , "<"
      , ":="
      ]
    , caseSensitive  = False
    }


-- Creating a lexer
lexer :: Token.TokenParser a
lexer =
  Token.makeTokenParser whileDef


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

braces :: Parser a -> Parser a
braces =
  Token.braces lexer

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


---------------------------------------------------------------------------------------------------
-- PARSING ----------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- EXPRESSIONS ------------------------------------------------------------------------------------

-- Arithmetic expressions --

-- buildExpressionParser does everything. It just needs what a term is and an OperatorTable.
arithExpr :: Parser ArithExpr
arithExpr =
  buildExpressionParser arithOp aTerm


-- specifying what is a term in the arithmetic expression
aTerm :: Parser ArithExpr
aTerm =
      parens arithExpr
  <|> AVar <$> identifier
  <|> AVal <$> integer


-- specifying operators precedence, loc (pre, in or post) and associativity
arithOp :: OperatorTable String () Identity ArithExpr
arithOp =
  [ [ Infix (reservedOp "*" *> pure (AOp Times )) AssocLeft
    , Infix (reservedOp "/" *> pure (AOp Divide)) AssocLeft
    ]
  , [ Infix (reservedOp "+" *> pure (AOp Plus  )) AssocLeft
    , Infix (reservedOp "-" *> pure (AOp Minus )) AssocLeft
    ]
  ]


-- Relational Expressions --

-- only supporting GT and LT
relationalOp :: Parser RelationalOp
relationalOp =
      (reservedOp ">" *> pure Gt)
  <|> (reservedOp "<" *> pure Lt)


-- no buildExpressionParser here :(
relationalExpr :: Parser BoolExpr
relationalExpr = do
  ae1 <- arithExpr
  op <- relationalOp
  ae2 <- arithExpr
  return $ A op ae1 ae2


-- Boolean Expression --

-- buildExpressionParser is back :) (Same thing as arithmetic expr)
boolExpr :: Parser BoolExpr
boolExpr =
  buildExpressionParser boolOp bTerm

-- specifying operators precedence, loc (pre, in or post) and associativity
boolOp :: OperatorTable String () Identity BoolExpr
boolOp =
  [ [ Infix (reservedOp "and" *> pure (B And)) AssocLeft
    , Infix (reservedOp "or"  *> pure (B Or )) AssocLeft
    ]
  ]


bTerm :: Parser BoolExpr
bTerm =
      parens boolExpr
  <|> (reserved "true"  *> pure (BVal True ))
  <|> (reserved "false" *> pure (BVal False))
  <|> relationalExpr


-- STATEMENTS -------------------------------------------------------------------------------------

-- Just direct translation of the grammer.
-- S ::= x := a | S1 ; S2 | if b then { S1 } else { S2 } | while b do { S }
statement :: Parser Statement
statement =
  let
    statement' :: Parser Statement
    statement' =
          assignment
      <|> ifthenelse
      <|> whiledo
  in
    Sequence <$> sepBy1 statement' semi


-- statement/statements enclosed in {}
statementBlock :: Parser Statement
statementBlock = braces statement


-- the assignment statement
assignment :: Parser Statement
assignment = do
  var <- identifier
  reserved ":="
  val <- arithExpr
  return $ Assign var val


-- if else statement
ifthenelse :: Parser Statement
ifthenelse = do
  reserved "if"
  boolE <- boolExpr
  reserved "then"
  thenBlock <- statementBlock
  reserved "else"
  elseBlock <- statementBlock
  return $ If boolE thenBlock elseBlock


-- while statement
whiledo :: Parser Statement
whiledo = do
  reserved "while"
  boolE <- boolExpr
  reserved "do"
  block <- statementBlock
  return $ While boolE block


-- the while parsers
whileLangParser :: Parser Statement
whileLangParser =
  whiteSpace *> statement


---------------------------------------------------------------------------------------------------
-- INTERPRETING LINE BY LINE ----------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- creating a way to store values
type State =
  M.Map String Integer


-- evaluate arithmetic expressions

evalAE :: State -> ArithExpr -> Integer
evalAE st (AVar s) = fromJust $ M.lookup s st
evalAE _  (AVal v) = v
evalAE st (AOp op e1 e2) =
  let
    e1' = evalAE st e1
    e2' = evalAE st e2
  in
    case op of
      Plus   -> e1' + e2'
      Minus  -> e1' - e2'
      Times  -> e1' * e2'
      Divide -> e1' `div` e2'


-- evaluateing boolean expression
evalBE :: State -> BoolExpr -> Bool
evalBE _ (BVal b) = b
evalBE st (B op e1 e2) =
  let
    e1' = evalBE st e1
    e2' = evalBE st e2
  in
    case op of
      And -> e1' && e2'
      Or  -> e1' || e2'

evalBE st (A op e1 e2) =
  let
    e1' = evalAE st e1
    e2' = evalAE st e2
  in
    case op of
      Gt -> e1' > e2'
      Lt -> e1' < e2'


-- evaluate statements --

-- Evaluate all the statements
eval :: State -> Statement -> State
eval st (Assign key e) = M.insert key (evalAE st e) st
eval st (While c ss) = if evalBE st c then eval (eval st ss) (While c ss) else st
eval st (If c tb eb) = if evalBE st c then eval st tb else eval st eb 
eval st (Sequence []) = st
eval st (Sequence (s:ss)) = eval (eval st s) (Sequence ss)


-- evaluate the program (which is basically an instance of type Statement) and generate a string.
evalAndShowVars :: Statement -> String
evalAndShowVars =
  unlines . map (\(x, y) -> x ++ " " ++ show y) . M.assocs . eval M.empty


-- MAIN --

{-
 - Here you have to design an interpreter for a subset of the While language. It is a simple
 - imperative language which only supports integer literals.
 - 
 - We will use similar grammar which its authors1,2,3 have used. Below is the description of
 - grammar that we will use.
 - 
 - x, y ∈ Var (variables)
 - n ∈ Num (numerals/integers)
 - 
 - opa ∈ Opa (arithmetic operators) 
 - oba ::= + | - | * | /
 - 
 - opb ∈ Opb (boolean operators) 
 - opb ::= and | or
 - 
 - opr ∈ Opr (relational operators) 
 - opr ::= > | <
 - 
 - a ∈ AExp (arithmetic expressions) 
 - a ::= x | n | a1 opa a2 | ( a )
 - 
 - b ∈ BExp (boolean expressions) 
 - b ::= true | false | b1 opb b2 | a1 opr a2 | ( b )
 - 
 - S ∈ Stmt (statements) 
 - S ::= x := a | S1 ; S2 | if b then { S1 } else { S2 } | while b do { S }
 - 
 - Here all operators are left associative. Their precedence order is as follows.
 - 
 - Arithmetic Operators: (*, /) > (+, -) > (>, <)
 - Boolean Operators: and > or
 - You can safely assume that all variables have integer type and are initialized properly. All
 - variables name will consist of only lowercase letter ('a'-'z') and it's length will not exceed
 - 10.
 - 
 - Note that ";" is more like of a sequencing operator. It is used to concatenate two statements.
 - That's why there will be no ";" at the end of block of statements.
 - 
 - All divisions are integers divisions, that is, a/b = floor(a/b). Intermediate values of any
 - variable will always be in range [0, 2*1018].
 - 
 - All test cases are valid programs. All of them will execute no more than 106 operations. All
 - operators and operand will be separated by at least one white space.
 - 
 - Input 
 - Input will be the multiline While program. You have to read it to the end of file.
 - 
 - Output 
 - At the end of program, you have to print each variable's name and its value, in different lines,
 - sorted by the lexicographical order of name.
 -}

main :: IO ()
main = do
  input <- getContents
  case parse whileLangParser "" input of
    Left err -> print err
    Right prog -> print (eval M.empty prog)

