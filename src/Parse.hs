module Parse where

import Prelude hiding (exp)
import Text.Parsec
import Data.Map as M hiding (map)
import Data.List

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

-- dirty operator precedence

opc :: Op -> Parser Op
opc op = try (keyword (opname op) >> return op)

opTable :: [[Op]]
opTable = [[LOR],
           [LAND],
           [OR],
           [XOR],
           [AND],
           [EQL, NEQ],
           [LSS, GTR, LEQ, GEQ],
           [SHL, SHR],
           [ADD, SUB],
           [MUL, DIV, MOD]]

binOpE' :: Int -> Parser Expr
binOpE' n = 
    if n == length opTable then
        choice $ map try [letE, trueE, falseE, ifE, varE, absE, intE, parens expr]
    else do
      let opE = binOpE' (n + 1)
      e1 <- opE
      rest <-binOpRemains e1 (opTable !! n) opE
      case rest of 
        Just binop -> return binop
        Nothing -> return e1

binOpE :: Parser Expr
binOpE = binOpE' 0

opParseSeqList :: [Op]
opParseSeqList = reverse $ sortBy cmp (concat opTable)
    where 
      cmp a b = compare (length (opname a)) (length (opname b))

-- expr suffix like + 1 - 2 + 3
binOpRemains :: Expr -> [Op] -> Parser Expr -> Parser (Maybe Expr)
binOpRemains e1 ops expP = do
  -- peek a token
  op <- optionMaybe . lookAhead . choice $ map opc $ opParseSeqList
  case op of
    Just op' ->
        -- if it is in ops
        case find (== op') ops of
          Just _ -> do
            -- consume the token
            _ <- choice $ map opc (concat opTable)
            e2 <- expP
            binOpRemains (BinOpE op' e1 e2) ops expP
          Nothing ->
            return $ Just e1
    Nothing ->
        return $ Just e1

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
