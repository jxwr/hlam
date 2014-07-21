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
  try (string "/*" >> many ((noneOf "*" >> return ()) <|> starNotCommentEnd) >> commentEnd)
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
  e1 <- expr
  keyword "then"
  e2 <- expr
  keyword "else"
  e3 <- expr
  return $ IfE e1 e2 e3

letE :: Parser Expr
letE = do
  v <- var
  keyword "="
  e <- expr
  return $ LetE v e

-- FIXME: Operator precedence

binOpP1 :: [Parser Op]
binOpP1 = map try [add', sub', and', or']
    where 
      add' = keyword "+" >> return Add
      sub' = keyword "-" >> return Sub
      and' = keyword "&&" >> return LAnd
      or' = keyword "||" >> return LOr

binOpP2 :: [Parser Op]
binOpP2 = map try [mul', div']
    where
      mul' = keyword "*" >> return Mul
      div' = keyword "/" >> return Div

binOpE2 :: Parser Expr
binOpE2 = do
  choice $ map try [letE, trueE, falseE, ifE, varE, absE, intE, parens expr]

-- (((1 + 2) + 3) - 4)
--- (BinOpE - (BinOpE + (BinOpE + 1 2)) 3) 4)
binOpE1 :: Parser Expr
binOpE1 = do
  e1 <- binOpE2
  rest <- binOpRemains e1 binOpP2 binOpE2
  case rest of 
    Just binop -> return binop
    Nothing -> return e1

binOpE :: Parser Expr
binOpE = do
  e1 <- binOpE1
  rest <-binOpRemains e1 binOpP1 binOpE1
  case rest of 
    Just binop -> return binop
    Nothing -> return e1

-- expr suffix like + 1 - 2 + 3
binOpRemains :: Expr -> [Parser Op] -> Parser Expr -> Parser (Maybe Expr)
binOpRemains e1 opP expP = do
  op <- optionMaybe $ choice opP
  case op of
    Just op' -> do
            e2 <- expP
            binOpRemains (BinOpE op' e1 e2) opP expP
    Nothing -> return $ Just e1

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
