module Parse where

import Prelude hiding (exp)
import Text.Parsec
import Data.Map as M

import Syntax
import TypeCheck
import Error
import Monad

type Parser a = Parsec String () a

surroundedBy :: Parser () -> Parser () -> Parser a -> Parser a
surroundedBy open close parser = do
    open
    separators
    x <- parser
    separators
    close
    return x

parens :: Parser a -> Parser a
parens = surroundedBy (char '(' >> return ()) (char ')' >> return ())

comment :: Parser ()
comment = do
  string "/*" >> many ((noneOf "*" >> return ()) <|> starNotCommentEnd) >> commentEnd
  return ()

starNotCommentEnd :: Parser ()
starNotCommentEnd = try (many1 (char '*') >> noneOf "/" >> return ())

commentEnd :: Parser ()
commentEnd = many1 (char '*') >> char '/' >> return ()

separator :: Parser ()
separator = (space >> return ()) <|> comment

separators :: Parser ()
separators = many separator >> return ()

symbol :: String -> Parser ()
symbol s = separators >> string s >> separators

keyword :: String -> Parser ()
keyword s = separators >> try (string s) >> separators

semi :: Parser ()
semi = separators >> char ';' >> separators

var :: Parser Var
var = do
  lit <- many1 letter
  return $ Var lit

intE :: Parser Exp
intE = do
  dig <- many1 digit
  return $ IntE (read dig :: Int)

trueE :: Parser Exp
trueE = do
  keyword "true" >> return TrueE

falseE :: Parser Exp
falseE = do
  keyword "false" >> return FalseE

varE :: Parser Exp
varE = do
  v <- var
  return $ VarE v

boolT :: Parser Type
boolT = do
  keyword "Bool" >> return BoolT

intT :: Parser Type
intT = do
  keyword "Int" >> return IntT

baseType :: Parser Type
baseType = do
  boolT <|> intT <|> parens baseType

typ :: Parser Type
typ = do
  ts <- sepBy1 baseType (keyword "->")
  return $ foldl1 FunT ts

absE :: Parser Exp
absE = do
  keyword "\\"
  v <- var
  keyword ":"
  t <- typ
  keyword "."
  e <- exp
  return $ AbsE v t e

ifE :: Parser Exp
ifE = do
  keyword "if"
  e1 <- simpleExp
  keyword "then"
  e2 <- simpleExp
  keyword "else"
  e3 <- simpleExp
  return $ IfE e1 e2 e3

-- FIXME: Operator precedence
binOpRest :: Exp -> Parser Exp
binOpRest e1 = do
  op <- (try add' <|> try sub' <|> try mul' <|> try div' <|> try and' <|> try or')
  e2 <- binOpE
  return $ BinOpE op e1 e2
    where 
      add' = keyword "+" >> return Add
      sub' = keyword "-" >> return Sub
      mul' = keyword "*" >> return Mul
      div' = keyword "/" >> return Div
      and' = keyword "&&" >> return LAnd
      or' = keyword "||" >> return LOr

binOpE :: Parser Exp
binOpE = do
  e1 <- simpleExp
  try (binOpRest e1) <|> return e1

simpleExp :: Parser Exp
simpleExp =
    trueE <|> 
    falseE <|> 
    ifE <|> 
    varE <|> 
    absE <|> 
    intE <|> 
    parens exp

exp :: Parser Exp
exp = do
  es <- sepBy1 binOpE separators
  return $ foldl1 AppE es

expStmt :: Parser Stmt
expStmt = do
  e <- exp
  return $ ExpStmt e

letStmt :: Parser Stmt
letStmt = do
  v <- var
  keyword "="
  e <- exp
  return $ LetStmt v e

stmt :: Parser Stmt
stmt = do
  try letStmt <|> expStmt 

parseStmts :: Parser [Stmt]
parseStmts = do
  separators
  stmts <- endBy1 stmt semi
  return stmts

parseHlam :: String -> String -> [HlamM Stmt]
parseHlam filename source = do
  let ctx = M.empty
  case parse parseStmts filename source of
    Left err -> [Left $ ParseError (show err)]
    Right stmts -> check ctx stmts
    where
      check :: TypeCheckContext -> [Stmt] -> [HlamM Stmt]
      check _ [] = []
      check ctx (x:xs) = 
          case typecheck ctx x of
            (Left err, ctx') -> Left err : (check ctx' xs)
            (Right _, ctx') -> Right x : (check ctx' xs)
