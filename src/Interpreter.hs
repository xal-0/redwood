{-# LANGUAGE RecursiveDo #-}

module Interpreter where

import Control.Monad.Except
import Control.Monad.State
import Data.IORef
import Data.List
import qualified Data.Map as M
import Runtime
import Syntax
import Utils

type Interpreter a = StateT Env (ExceptT Error IO) a

-- | Mappings from variable names to built-in functions.  Programs get
-- these bindings in their environment when they start.
initialEnv :: Env
initialEnv =
  Env $
    M.fromList
      [ ("print", ValuePrim PrimPrint)
      ]

testBlock :: Block
testBlock =
  [ StmtAssign (ExprVariable "x") (ExprArray [ExprNumber 3]),
    StmtAssign (ExprIndex (ExprVariable "x") (ExprNumber 0)) (ExprNumber 4),
    StmtExpr (ExprCall (ExprVariable "print") [ExprVariable "x"])
  ]

testInterpret :: Block -> IO (Either Error Value)
testInterpret b = runExceptT (evalStateT (evalBlock b) initialEnv)

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
evalStmt s@(StmtWhile condition conditional) = do
  conditionValue <- evalExpr condition
  conditionBool <- checkBool conditionValue
  if conditionBool
    then evalBlock conditional >> evalStmt s
    else pure ValueNull
evalStmt (StmtExpr e) = evalExpr e
-- Need a recursive do when doing a function definition, since
-- functions can refer to themselves.  We first insert the evaluated
-- expression into the environment, and only then evaluate the
-- expression.  Laziness wins again!
evalStmt (StmtFunc v ps body) = mdo
  modify (\(Env m) -> Env (M.insert v func m))
  func <- evalExpr (ExprFunc ps body)
  pure ValueNull
evalStmt (StmtAssign (ExprVariable v) e) = do
  e' <- evalExpr e
  modify (\(Env m) -> Env (M.insert v e' m))
  pure ValueNull
-- Assigning to a dictionary/array can't be recursive, because they're
-- on the heap.
evalStmt (StmtAssign (ExprIndex ref index) e) = do
  ref' <- evalExpr ref >>= checkRef
  index' <- evalExpr index
  obj <- liftIO (readIORef ref')
  e' <- evalExpr e
  case obj of
    ObjectArray a -> do
      i <- checkInt index'
      case replaceIdx a i e' of
        Nothing -> throwError ErrIndex
        Just a' -> do
          liftIO (writeIORef ref' (ObjectArray a'))
    ObjectDict d -> do
      _ <- checkKey index'
      let d' = M.insert index' e' d
      liftIO (writeIORef ref' (ObjectDict d'))
  pure ValueNull
evalStmt (StmtAssign _ _) = throwError ErrAssign
evalStmt (StmtReturn Nothing) = pure ValueNull
evalStmt (StmtReturn (Just r)) = evalExpr r

evalExpr :: Expr -> Interpreter Value
evalExpr (ExprVariable v) = do
  Env m <- get
  maybe (throwError (ErrLookup v)) pure (M.lookup v m)
evalExpr (ExprNumber n) = pure (ValueNumber n)
evalExpr (ExprString n) = pure (ValueString n)
evalExpr (ExprBool n) = pure (ValueBool n)
evalExpr (ExprBinop op x y) = do
  x' <- evalExpr x
  y' <- evalExpr y
  evalBinop op x' y'
evalExpr (ExprMonop op x) = do
  x' <- evalExpr x
  evalMonop op x'
evalExpr (ExprCall f as) = do
  f' <- evalExpr f
  as' <- traverse evalExpr as
  case f' of
    ValueClosure env params body -> apply env params body as'
    ValuePrim p -> evalPrim p as'
    _ -> throwError (ErrType VTypeClosure (valueType f'))
evalExpr (ExprFunc ps body) = do
  env <- get
  pure (ValueClosure env ps body)
evalExpr (ExprArray exprs) = do
  exprs' <- traverse evalExpr exprs
  ref <- liftIO (newIORef (ObjectArray exprs'))
  pure (ValueRef ref)
evalExpr (ExprDict entries) = do
  entries' <- traverse evalEntry entries
  ref <- liftIO (newIORef (ObjectDict (M.fromList entries')))
  pure (ValueRef ref)
  where
    evalEntry (k, v) = (,) <$> evalExpr k <*> evalExpr v
evalExpr (ExprIfElseChain [] Nothing) = pure ValueNull
evalExpr (ExprIfElseChain [] (Just els)) = evalBlock els
evalExpr (ExprIfElseChain ((cond, body) : xs) els) = do
  conditionValue <- evalExpr cond
  conditionBool <- checkBool conditionValue
  if conditionBool
    then evalBlock body
    else evalExpr (ExprIfElseChain xs els)
evalExpr (ExprIndex ref index) = do
  obj <- evalExpr ref >>= checkRef >>= liftIO . readIORef
  index' <- evalExpr index
  case obj of
    ObjectArray a -> do
      i <- checkInt index'
      maybe (throwError ErrIndex) pure (a !!? i)
    ObjectDict d -> do
      _ <- checkKey index'
      maybe (throwError ErrIndex) pure (M.lookup index' d)

evalPrim :: Prim -> [Value] -> Interpreter Value
evalPrim PrimPrint as = do
  strs <- traverse showValue as
  liftIO (putStrLn (concat strs))
  pure ValueNull

-- operations between two values
evalBinop :: Binop -> Value -> Value -> Interpreter Value
evalBinop BinopPlus x y = binopCheck checkNumber ValueNumber (+) x y
evalBinop BinopMinus x y = binopCheck checkNumber ValueNumber (-) x y
evalBinop BinopExp x y = binopCheck checkNumber ValueNumber (**) x y
evalBinop BinopMult x y = binopCheck checkNumber ValueNumber (*) x y
evalBinop BinopDiv x y = binopCheck checkNumber ValueNumber (/) x y
evalBinop BinopMod x y = binopCheck checkInt (ValueNumber . fromIntegral) mod x y
evalBinop BinopLessThan x y = binopCheck checkKey ValueBool (<) x y
evalBinop BinopGreaterThan x y = binopCheck checkKey ValueBool (>) x y
evalBinop BinopGreaterThanEq x y = binopCheck checkKey ValueBool (>=) x y
evalBinop BinopLessThanEq x y = binopCheck checkKey ValueBool (<=) x y
evalBinop BinopEq x y = binopCheck checkKey ValueBool (==) x y
evalBinop BinopNotEq x y = binopCheck checkKey ValueBool (/=) x y
evalBinop BinopAnd x y = binopCheck checkBool ValueBool (&&) x y
evalBinop BinopOr x y = binopCheck checkBool ValueBool (||) x y

-- operations on one value
evalMonop :: Monop -> Value -> Interpreter Value
evalMonop MonopNot x = monopCheck checkBool ValueBool not x
evalMonop MonopNeg x = monopCheck checkNumber ValueNumber negate x

-- | Given a "check" function, that turns a value into an "a" (and
-- potentially fails), a binary operation that takes two "a"s to a
-- "b", and a way to get a Value from a "b", create a type-checked
-- binary operation on Values.
binopCheck ::
  (Value -> Interpreter a) ->
  (b -> Value) ->
  (a -> a -> b) ->
  Value ->
  Value ->
  Interpreter Value
binopCheck check result op x y = do
  x' <- check x
  y' <- check y
  if valueType x /= valueType y
    then throwError (ErrType (valueType x) (valueType y))
    else pure ()
  pure (result (x' `op` y'))

monopCheck ::
  (Value -> Interpreter a) ->
  (b -> Value) ->
  (a -> b) ->
  Value ->
  Interpreter Value
monopCheck check result op x = do
  x' <- check x
  pure (result (op x'))

showValue :: Value -> Interpreter String
showValue (ValueNumber n) = pure (show n)
showValue (ValueString n) = pure n
showValue (ValueBool b) = pure (if b then "true" else "false")
showValue (ValueClosure _ _ _) = pure "<closure>"
showValue ValueNull = pure "null"
showValue (ValuePrim _) = pure "<primitive>"
showValue (ValueRef r) = do
  obj <- liftIO (readIORef r)
  case obj of
    ObjectArray values -> do
      strs <- traverse showValue values
      pure ("[" ++ intercalate ", " strs ++ "]")
    ObjectDict entries -> do
      let showEntry (k, v) = do
            k' <- case k of
              ValueString s -> pure s
              _ -> showValue k
            v' <- showValue v
            pure (k' ++ ": " ++ v')
      strs <- traverse showEntry (M.toList entries)
      pure ("{" ++ intercalate ", " strs ++ "}")

checkNumber :: Value -> Interpreter Double
checkNumber (ValueNumber n) = pure n
checkNumber v = throwError (ErrType VTypeNumber (valueType v))

checkInt :: Value -> Interpreter Int
checkInt x = do
  num <- checkNumber x
  if num /= fromIntegral (round num :: Int)
    then throwError ErrIndex
    else pure (round num)

checkBool :: Value -> Interpreter Bool
checkBool (ValueBool n) = pure n
checkBool v = throwError (ErrType VTypeBool (valueType v))

checkRef :: Value -> Interpreter (IORef Object)
checkRef (ValueRef r) = pure r
checkRef v = throwError (ErrType VTypeBool (valueType v))

-- | Make sure that the value is a key type.
checkKey :: Value -> Interpreter Value
checkKey v@(ValueNumber _) = pure v
checkKey v@(ValueString _) = pure v
checkKey ValueNull = pure ValueNull
checkKey _ = throwError ErrKey

valueType :: Value -> VType
valueType (ValueNumber _) = VTypeNumber
valueType (ValueBool _) = VTypeBool
valueType (ValueString _) = VTypeString
valueType ValueClosure {} = VTypeClosure
valueType ValueNull = VTypeNull
valueType (ValueRef _) = VTypeRef -- TODO
valueType (ValuePrim _) = VTypePrim
