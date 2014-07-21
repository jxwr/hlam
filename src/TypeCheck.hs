module TypeCheck where

import qualified Data.Map as M
import Data.List
import Data.Maybe

import Syntax
import Monad
import Error

type TypeCheckContext = M.Map String Type

typecheckExpr :: TypeCheckContext -> Expr -> HlamM Type
typecheckExpr ctx (VarE v) = 
    case M.lookup (unVar v) ctx of
      Just t -> Right t
      Nothing -> Left $ TypeError $ "No type info for " ++ show v

typecheckExpr ctx (BinOpE op e1 e2) = do
  t1 <- typecheckExpr ctx e1
  t2 <- typecheckExpr ctx e2

  if isIntOp op then checkIntOp t1 t2 op else checkLogicOp t1 t2 op
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
typecheckExpr ctx (AbsE v t e) = do
  let nc = M.insert (unVar v) t ctx
  et <- typecheckExpr nc e
  return $ FunT t et

typecheckExpr ctx (AppE e1 e2) = do
  t1 <- typecheckExpr ctx e1
  t2 <- typecheckExpr ctx e2
         
  case t1 of
    BoolT -> Left $ TypeError $ show e1 ++ " is not a function"
    IntT -> Left $ TypeError $ show e1 ++ " is not a function"
    FunT fromT toT -> 
        if fromT /= t2 then 
            Left $ TypeError $ "function " ++ (show e1) ++ " need " ++ (show fromT) ++ " typed argument, but " ++ (show t2) ++ " given "
        else
            Right toT

typecheckExpr _ (IntE _) = Right IntT
typecheckExpr _ TrueE = Right BoolT
typecheckExpr _ FalseE = Right BoolT

typecheckExpr ctx (IfE e1 e2 e3) = do
  t1 <- typecheckExpr ctx e1
  t2 <- typecheckExpr ctx e2
  t3 <- typecheckExpr ctx e3

  if t1 /= BoolT || t2 /= t3 then
      Left $ TypeError "if expr type error"
  else
      Right t2

typecheck :: TypeCheckContext -> Expr -> (HlamM Type, TypeCheckContext)
typecheck ctx (LetE v e) = case typecheckExpr ctx e of
      Left err -> (Left err, ctx)
      Right t -> (Right t, M.insert (unVar v) t ctx)

typecheck ctx e = (typecheckExpr ctx e, ctx)

isIntOp :: Op -> Bool
isIntOp op = isJust $ find (== op) [MUL, DIV, MOD, ADD, SUB, SHL, SHR, AND, XOR, OR]

isLogicOp :: Op -> Bool
isLogicOp op = isJust $ find (== op) [LSS, GTR, LEQ, GEQ, LAND, LOR]
