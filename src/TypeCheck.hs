module TypeCheck where

import qualified Data.Map as M
import Syntax
import Monad
import Error

type TypeCheckContext = M.Map String Type
type TypeError = String

typecheckExp :: TypeCheckContext -> Exp -> HlamM Type
typecheckExp ctx (VarE v) = 
    case M.lookup (unVar v) ctx of
      Just t -> Right t
      Nothing -> Left $ TypeError $ "No type info for " ++ show v

typecheckExp ctx (BinOpE op e1 e2) = do
  t1 <- typecheckExp ctx e1
  t2 <- typecheckExp ctx e2

  case op of
    Add -> checkIntOp t1 t2 Add
    Sub -> checkIntOp t1 t2 Sub
    Mul -> checkIntOp t1 t2 Mul
    Div -> checkIntOp t1 t2 Div
    LAnd -> checkLogicOp t1 t2 LAnd
    LOr -> checkLogicOp t1 t2 LOr
    where
      checkIntOp t1 t2 op' = 
          if t1 == IntT && t2 == IntT then
              Right IntT 
          else 
              Left $ TypeError $ 
                   "'" ++ show op' ++ "' need two IntT operands, " ++ 
                   show t1 ++ " and " ++ show t2 ++ " given"
      checkLogicOp t1 t2 op' = 
          if t1 == BoolT && t2 == BoolT then
              Right BoolT
          else 
              Left $ TypeError $ 
                   "'" ++ show op' ++ "' need two BoolT operands, " ++ 
                   show t1 ++ " and " ++ show t2 ++ " given"

-- (\x : Bool . x) true
typecheckExp ctx (AbsE v t e) = do
  let nc = M.insert (unVar v) t ctx
  et <- typecheckExp nc e
  return $ FunT t et

typecheckExp ctx (AppE e1 e2) = do
  t1 <- typecheckExp ctx e1
  t2 <- typecheckExp ctx e2
         
  case t1 of
    BoolT -> Left $ TypeError $ show e1 ++ " is not a function"
    IntT -> Left $ TypeError $ show e1 ++ " is not a function"
    FunT fromT toT -> 
        if fromT /= t2 then 
            Left $ TypeError $ "function " ++ (show e1) ++ " need " ++ (show fromT) ++ " typed argument, but " ++ (show t2) ++ " given "
        else
            Right toT

typecheckExp _ (IntE _) = Right IntT
typecheckExp _ TrueE = Right BoolT
typecheckExp _ FalseE = Right BoolT

typecheckExp ctx (IfE e1 e2 e3) = do
  t1 <- typecheckExp ctx e1
  t2 <- typecheckExp ctx e2
  t3 <- typecheckExp ctx e3

  if t1 /= BoolT || t2 /= t3 then
      Left $ TypeError "if expr type error"
  else
      Right t2

typecheck :: TypeCheckContext -> Stmt -> (HlamM Type, TypeCheckContext)
typecheck ctx (ExpStmt e) = 
    (typecheckExp ctx e, ctx)

typecheck ctx (LetStmt v e) =
    case typecheckExp ctx e of
      Left err -> (Left err, ctx)
      Right t -> (Right t, M.insert (unVar v) t ctx)
