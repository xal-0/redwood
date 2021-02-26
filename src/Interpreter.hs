module Interpreter where

import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import Data.List
import qualified Data.Map as M
import Runtime
import Syntax
import Utils

type Interpreter a = ReaderT (IORef Env) (ExceptT Error IO) a

-- | Mappings from variable names to built-in functions.  Programs get
-- these bindings in their environment when they start.
initialEnv :: Env
initialEnv =
  Env
    ( M.fromList
        [ ("print", ValuePrim PrimPrint)
        ]
    )
    Nothing

testBlock :: Block
testBlock =
  [ StmtAssign (ExprVariable "x") (ExprArray [ExprNumber 3]),
    StmtAssign (ExprIndex (ExprVariable "x") (ExprNumber 0)) (ExprNumber 4),
    StmtExpr (ExprCall (ExprVariable "print") [ExprVariable "x"])
  ]

testInterpret :: Block -> IO (Either Error Value)
testInterpret b = do
  env <- newIORef initialEnv
  runExceptT (runReaderT (evalBlock b) env)

apply :: IORef Env -> [Ident] -> Block -> [Value] -> Interpreter Value
apply env params body args
  | length params /= length args =
    throwError
      (ErrArgs (length params) (length args))
  | otherwise = do
    let argBinds = M.fromList (zip params args)
    env' <- liftIO (newIORef (Env argBinds (Just env)))
    local (const env') (evalBlock body)

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
evalStmt (StmtFunc v ps body) =
  evalStmt (StmtAssign (ExprVariable v) (ExprFunc ps body))
evalStmt (StmtAssign (ExprVariable v) e) = do
  e' <- evalExpr e
  modifyEnv v e'
  pure ValueNull
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
evalExpr (ExprVariable v) = lookupEnv v
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
  env <- ask
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

lookupEnv :: Ident -> Interpreter Value
lookupEnv var = do
  result <- findEnv var
  case result of
    Just (_, val) -> pure val
    Nothing -> throwError (ErrLookup var)

modifyEnv :: Ident -> Value -> Interpreter ()
modifyEnv var val = do
  result <- findEnv var
  ref <- case result of
    Just (ref, _) -> pure ref
    Nothing -> ask
  Env locals parent <- liftIO (readIORef ref)
  let env' = Env (M.insert var val locals) parent
  liftIO (writeIORef ref env')

-- | Return a reference to the envinorment this variable is defined
-- in, and Nothing if it is not defined in any of them.
findEnv :: Ident -> Interpreter (Maybe (IORef Env, Value))
findEnv var = do
  ref <- ask
  Env locals parent <- liftIO (readIORef ref)
  case M.lookup var locals of
    Just val -> pure (Just (ref, val))
    Nothing -> case parent of
      Just ref' -> do
        local (const ref') (findEnv var)
      Nothing -> pure Nothing

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
