module Parse where

import Prelude hiding (exp)
import Text.Parsec
import Data.Map as M hiding (map)

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

intE :: Parser Expr
intE = do
  dig <- many1 digit
  return $ IntE (read dig :: Int)

trueE :: Parser Expr
trueE = do
  keyword "true" >> return TrueE

falseE :: Parser Expr
falseE = do
  keyword "false" >> return FalseE

varE :: Parser Expr
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

absE :: Parser Expr
absE = do
  keyword "\\"
  v <- var
  keyword ":"
  t <- typ
  keyword "."
  e <- expr
  return $ AbsE v t e

ifE :: Parser Expr
ifE = do
  keyword "if"
  e1 <- simpleExpr
  keyword "then"
  e2 <- simpleExpr
  keyword "else"
  e3 <- simpleExpr
  return $ IfE e1 e2 e3

-- FIXME: Operator precedence
binOpRest :: Expr -> Parser Expr
binOpRest e1 = do
  op <- choice $ map try [add', sub', mul', div', and', or']
  e2 <- binOpE
  return $ BinOpE op e1 e2
    where 
      add' = keyword "+" >> return Add
      sub' = keyword "-" >> return Sub
      mul' = keyword "*" >> return Mul
      div' = keyword "/" >> return Div
      and' = keyword "&&" >> return LAnd
      or' = keyword "||" >> return LOr

binOpE :: Parser Expr
binOpE = do
  e1 <- simpleExpr
  try (binOpRest e1) <|> return e1

simpleExpr :: Parser Expr
simpleExpr = choice $ map try [letE, trueE, falseE, ifE, varE, absE, intE, parens expr]

letE :: Parser Expr
letE = do
  v <- var
  keyword "="
  e <- expr
  return $ LetE v e

expr :: Parser Expr
expr = do
  es <- sepBy1 binOpE separators
  return $ foldl1 AppE es

exprs :: Parser [Expr]
exprs = do
  separators
  es <- endBy1 expr semi
  return es

parseHlam :: String -> String -> [HlamM Expr]
parseHlam filename source = do
  let ctx = M.empty
  case parse exprs filename source of
    Left err -> [Left $ ParseError (show err)]
    Right exps -> check ctx exps
    where
      check :: TypeCheckContext -> [Expr] -> [HlamM Expr]
      check _ [] = []
      check ctx (x:xs) = 
          case typecheck ctx x of
            (Left err, ctx') -> Left err : (check ctx' xs)
            (Right _, ctx') -> Right x : (check ctx' xs)
