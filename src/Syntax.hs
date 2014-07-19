module Syntax where

newtype Var = Var { unVar :: String }
    deriving (Eq)

data Exp = VarE Var
         | AbsE Var Type Exp -- \x : Bool . x
         | AppE Exp Exp
         | TrueE
         | FalseE
         | IfE Exp Exp Exp   -- if true then false else true

data Type = BoolT
          | FunT Type Type
            deriving (Eq)

data Stmt = ExpStmt Exp
          | LetStmt Var Exp

instance Show Var where
    show (Var v) = v

instance Show Exp where
    show (VarE v) = show v
    show (AbsE v t e) = "(\\" ++ show v ++ " : " ++ show t ++ " -> " ++ show e ++ ")"
    show (AppE e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show TrueE = "true"
    show FalseE = "false"
    show (IfE e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3 ++ ")"

instance Show Type where
    show BoolT = "Bool"
    show (FunT t1 t2) = show t1 ++ "->" ++ show t2

instance Show Stmt where
    show (ExpStmt e) = show e
    show (LetStmt v e) = show v ++ " = " ++ show e
