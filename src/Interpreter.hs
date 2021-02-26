{-# LANGUAGE RecursiveDo #-}

module Interpreter where

import Control.Monad.Except
import Control.Monad.State
import Data.IORef
import Data.List
import qualified Data.Map as M
import Syntax
import Utils

newtype Env = Env (M.Map Ident Value)

-- | An immutable value at runtime.  Variables bind to these directly:
-- assigning to a variable changes which value it points to.  (If you
-- pass a value to a function, it is "copied" in.)
data Value
  = ValueNumber Double
  | ValueBool Bool
  | ValueString String
  | -- | A closure.  When you evaluate a functione expression, it
    -- | closes over its local envinrionment and returns it in one of
    -- | these (lexical scope).
    ValueClosure Env [Ident] Block
  | ValueNull
  | -- | A reference to an object on the heap.  This is for variables
    -- | that refer to things that can be mutated, like arrays or
    -- | dictionaries.  When you pass a reference into a function, the
    -- | reference is copied, but the object that it points to is not.
    -- | Assigning to a dictionary/array access expression mutates the
    -- | object pointed to by the reference.
    ValueRef (IORef Object)
  | -- | A builtin value, like print.
    ValuePrim Prim

instance Eq Value where
  ValueNumber x == ValueNumber y = x == y
  ValueBool x == ValueBool y = x == y
  ValueString x == ValueString y = x == y
  ValueNull == ValueNull = True
  _ == _ = error "equality is not defined on function or reference types"

instance Ord Value where
  ValueNumber x `compare` ValueNumber y = x `compare` y
  ValueBool x `compare` ValueBool y = x `compare` y
  ValueString x `compare` ValueString y = x `compare` y
  ValueNull `compare` ValueNull = EQ
  _ `compare` _ = error "equality is not defined on function or reference types"

-- | An object on the heap.  Functions that modify objects can write
-- to the IORef pointing to us.
data Object
  = ObjectArray [Value]
  | ObjectDict (M.Map Value Value)

-- | A built-in value, like the print function.
data Prim
  = PrimPrint

-- | Mappings from variable names to built-in functions.  Programs get
-- these bindings in their environment when they start.
initialEnv :: Env
initialEnv =
  Env $
    M.fromList
      [ ("print", ValuePrim PrimPrint)
      ]

data VType
  = VTypeNumber
  | VTypeBool
  | VTypeString
  | VTypeClosure
  | VTypeNull
  | VTypeRef
  | VTypePrim
  deriving (Show, Eq)

data Error
  = ErrLookup Ident
  | ErrType VType VType
  | ErrArgs Int Int
  | ErrAssign
  | ErrKey
  | ErrIndex
  deriving (Show)

type Interpreter a = StateT Env (ExceptT Error IO) a

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
      checkKey index'
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
      checkKey index'
      maybe (throwError ErrIndex) pure (M.lookup index' d)

evalPrim :: Prim -> [Value] -> Interpreter Value
evalPrim PrimPrint as = do
  strs <- traverse showValue as
  liftIO (putStrLn (concat strs))
  pure ValueNull

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

showValue :: Value -> Interpreter String
showValue (ValueNumber n) = pure (show n)
showValue (ValueString n) = pure n
showValue (ValueBool b) = pure (if b then "true" else "false")
showValue (ValueClosure _ _ _) = pure "<closure>"
showValue ValueNull = pure "null"
showValue (ValuePrim p) = pure "<primitive>"
showValue (ValueRef r) = do
  obj <- liftIO (readIORef r)
  case obj of
    ObjectArray values -> do
      strs <- traverse showValue values
      pure ("[" ++ intercalate ", " strs ++ "]")

checkNumber :: Value -> Interpreter Double
checkNumber (ValueNumber n) = pure n
checkNumber v = throwError (ErrType VTypeNumber (valueType v))

checkInt :: Value -> Interpreter Int
checkInt x = do
  num <- checkNumber x
  if num /= fromIntegral (round num)
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
