module Syntax where

import Data.Map as M hiding (map)

newtype Var = Var { unVar :: String } deriving (Eq)

data Expr = VarE Var
         | IntE Int
         | AbsE Var Type Expr -- \x : Bool . x
         | AppE Expr Expr
         | TrueE
         | FalseE
         | IfE Expr Expr Expr   -- if true then false else true
         | BinOpE Op Expr Expr
         | LetE Var Expr

data Op = MUL | DIV | MOD 
        | ADD | SUB  
        | SHL | SHR 
        | LSS | GTR | LEQ | GEQ 
        | EQL | NEQ 
        | AND
        | XOR
        | OR
        | LAND 
        | LOR
          deriving (Eq, Ord)

opNameMap :: M.Map Op String 
opNameMap = M.fromList [
         (MUL, "*"), (DIV, "/"), (MOD, "%"),
         (ADD, "+"), (SUB, "-"),
         (SHR, ">>"), (SHL, "<<"),
         (LSS, "<"), (GTR, ">"), (LEQ, "<="), (GEQ, ">="),
         (EQL, "=="), (NEQ, "/="), 
         (AND, "&"), 
         (XOR, "^"),
         (OR, "|"),
         (LAND, "&&"),
         (LOR, "||")
        ]

opname :: Op -> String
opname op = case M.lookup op opNameMap of
              Just s -> s
              Nothing -> "Unknown Operator"

data Type = BoolT
          | IntT
          | FunT Type Type
            deriving (Eq)

instance Show Var where
    show (Var v) = v

instance Show Expr where
    show (VarE v) = show v
    show (IntE v) = show v
    show (AbsE v t e) = "(\\" ++ show v ++ " : " ++ show t ++ " -> " ++ show e ++ ")"
    show (AppE e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show TrueE = "true"
    show FalseE = "false"
    show (IfE e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3 ++ ")"
    show (BinOpE op e1 e2) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"
    show (LetE v e) = show v ++ " = " ++ show e

instance Show Op where
    show op = opname op

instance Show Type where
    show BoolT = "Bool"
    show IntT = "Int"
    show (FunT t1 t2) = show t1 ++ "->" ++ show t2
