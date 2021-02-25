{-# LANGUAGE RecursiveDo #-}

module Interpreter where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import Syntax

newtype Env = Env (M.Map Ident Value)
  deriving (Show)

data Value
  = ValueNumber Double
  | ValueClosure Env [Ident] Block
  | ValueNull
  deriving (Show)

data VType
  = VTypeNumber
  | VTypeClosure
  | VTypeNull
  deriving (Show)

data Error
  = ErrLookup Ident
  | ErrType VType VType
  | ErrArgs Int Int
  deriving (Show)

type Interpreter a = StateT Env (ExceptT Error IO) a

testInterpret :: Block -> IO (Either Error Value)
testInterpret b = runExceptT (evalStateT (evalBlock b) (Env M.empty))

apply :: Env -> [Ident] -> Block -> [Value] -> Interpreter Value
apply (Env env) params body args
  | length params /= length args =
    throwError
      (ErrArgs (length params) (length args))
  | otherwise = do
    let argBinds = M.fromList (zip params args)
        env' = Env (argBinds `M.union` env)
    lift (evalStateT (evalBlock body) env')

evalBlock :: Block -> Interpreter Value
evalBlock [] = pure ValueNull
evalBlock (s@(StmtReturn _) : _) = evalStmt s
evalBlock [stmt] = evalStmt stmt
evalBlock (stmt : stmts) = evalStmt stmt >> evalBlock stmts

evalStmt :: Stmt -> Interpreter Value
evalStmt (StmtExpr e) = evalExpr e
evalStmt (StmtAssign v e) = mdo
  modify (\(Env m) -> Env (M.insert v e' m))
  e' <- evalExpr e
  pure ValueNull
evalStmt (StmtReturn Nothing) = pure ValueNull
evalStmt (StmtReturn (Just r)) = evalExpr r

evalExpr :: Expr -> Interpreter Value
evalExpr (ExprVariable v) = do
  Env m <- get
  maybe (throwError (ErrLookup v)) pure (M.lookup v m)
evalExpr (ExprNumber n) = pure (ValueNumber n)
evalExpr (ExprBinop op x y) = do
  x' <- evalExpr x
  y' <- evalExpr y
  evalBinop op x' y'
evalExpr (ExprCall f as) = do
  (env, params, body) <- evalExpr f >>= checkClosure
  as' <- traverse evalExpr as
  apply env params body as'
evalExpr (ExprFunc ps body) = do
  env <- get
  pure (ValueClosure env ps body)

evalBinop :: Binop -> Value -> Value -> Interpreter Value
evalBinop BinopPlus x y = do
  x' <- checkNumber x
  y' <- checkNumber y
  pure (ValueNumber (x' + y'))

checkNumber :: Value -> Interpreter Double
checkNumber (ValueNumber n) = pure n
checkNumber v = throwError (ErrType VTypeNumber (valueType v))

checkClosure :: Value -> Interpreter (Env, [Ident], Block)
checkClosure (ValueClosure env params body) = pure (env, params, body)
checkClosure v = throwError (ErrType VTypeClosure (valueType v))

valueType :: Value -> VType
valueType (ValueNumber _) = VTypeNumber
valueType ValueClosure {} = VTypeClosure
valueType ValueNull = VTypeNull
