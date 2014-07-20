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
    _ <- string "/*"
    _ <- many ((noneOf "*" >> return ()) <|> starNotCommentEnd)
    commentEnd
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

parseVar :: Parser Var
parseVar = do
  lit <- many1 letter
  return $ Var lit

parseIntE :: Parser Exp
parseIntE = do
  dig <- many1 digit
  return $ IntE (read dig :: Int)

parseTrueE :: Parser Exp
parseTrueE = do
  keyword "true" >> return TrueE

parseFalseE :: Parser Exp
parseFalseE = do
  keyword "false" >> return FalseE

parseVarE :: Parser Exp
parseVarE = do
  var <- parseVar
  return $ VarE var

parseBoolT :: Parser Type
parseBoolT = do
  keyword "Bool" >> return BoolT

parseIntT :: Parser Type
parseIntT = do
  keyword "Int" >> return IntT

parseBaseType :: Parser Type
parseBaseType = do
  parseBoolT <|> parseIntT <|> parens parseBaseType

parseType :: Parser Type
parseType = do
  ts <- sepBy1 parseBaseType (keyword "->")
  return $ foldl1 FunT ts

parseAbsE :: Parser Exp
parseAbsE = do
  keyword "\\"
  var <- parseVar
  keyword ":"
  typ <- parseType
  keyword "."
  exp <- parseExp
  return $ AbsE var typ exp

parseIfE :: Parser Exp
parseIfE = do
  keyword "if"
  e1 <- parseSimpleExp
  keyword "then"
  e2 <- parseSimpleExp
  keyword "else"
  e3 <- parseSimpleExp
  return $ IfE e1 e2 e3

-- FIXME: Operator precedence
parseBinOpRest :: Exp -> Parser Exp
parseBinOpRest e1 = do
  op <- (try parseAdd <|> try parseSub <|> try parseMul <|> try parseDiv <|> try parseLAnd <|> try parseLOr)
  e2 <- parseBinOpE
  return $ BinOpE op e1 e2
    where 
      parseAdd = keyword "+" >> return Add
      parseSub = keyword "-" >> return Sub
      parseMul = keyword "*" >> return Mul
      parseDiv = keyword "/" >> return Div
      parseLAnd = keyword "&&" >> return LAnd
      parseLOr = keyword "||" >> return LOr

parseBinOpE :: Parser Exp
parseBinOpE = do
  e1 <- parseSimpleExp
  try (parseBinOpRest e1) <|> return e1

parseSimpleExp :: Parser Exp
parseSimpleExp =
    parseTrueE <|> 
    parseFalseE <|> 
    parseIfE <|> 
    parseVarE <|> 
    parseAbsE <|> 
    parseIntE <|> 
    parens parseExp

parseExp :: Parser Exp
parseExp = do
  es <- sepBy1 parseBinOpE separators
  return $ foldl1 AppE es

parseExpStmt :: Parser Stmt
parseExpStmt = do
  exp <- parseExp
  return $ ExpStmt exp

parseLetStmt :: Parser Stmt
parseLetStmt = do
  var <- parseVar
  keyword "="
  exp <- parseExp
  return $ LetStmt var exp

parseStmt :: Parser Stmt
parseStmt = do
  try parseLetStmt <|> parseExpStmt 

parseStmts :: Parser [Stmt]
parseStmts = do
  separators
  stmts <- endBy1 parseStmt semi
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
