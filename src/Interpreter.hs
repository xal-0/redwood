module Interpreter where

import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Parser
import Runtime
import Syntax
import Utils

makeInterpreter :: [(Ident, Prim)] -> IO Interpreter
makeInterpreter extraBuiltins = do
  let builtins =
        fmap
          (\(i, p) -> (i, ValuePrim p))
          (extraBuiltins ++ initialPrims)
      env = Env (M.fromList builtins) Nothing
  ref <- newIORef env
  pure (Interpreter ref)

runInterpret :: Interpreter -> Interpret a -> IO (Maybe a)
runInterpret (Interpreter ref) int = do
  result <- runExceptT (runReaderT int ref)
  case result of
    Left err -> do
      print err
      pure Nothing
    Right a -> pure (Just a)

evalSource :: Interpreter -> FilePath -> IO Bool
evalSource interpreter path = do
  block <- parseBlock path
  case block of
    Nothing -> pure False
    Just b -> fmap isJust (runInterpret interpreter (evalBlock b))

evalCall :: Interpreter -> String -> IO (Maybe Value)
evalCall interpreter var =
  let prog = StmtExpr (ExprCall (ExprVariable var) [])
   in runInterpret interpreter (evalStmt prog)

-- | Mappings from variable names to built-in functions.  Programs get
-- these bindings in their environment when they start.
initialPrims :: [(Ident, Prim)]
initialPrims =
  [ ("println", evalPrint True),
    ("print", evalPrint False)
  ]

apply :: IORef Env -> [Ident] -> Block -> [Value] -> Interpret Value
apply env params body args
  | length params /= length args =
    throwError
      (ErrArgs (length params) (length args))
  | otherwise = do
    let argBinds = M.fromList (zip params args)
    env' <- liftIO (newIORef (Env argBinds (Just env)))
    local (const env') (evalBlock body)

evalBlock :: Block -> Interpret Value
evalBlock [] = pure ValueNull
evalBlock (s@(StmtReturn _) : _) = evalStmt s
evalBlock [stmt] = evalStmt stmt
evalBlock (stmt : stmts) = evalStmt stmt >> evalBlock stmts

evalStmt :: Stmt -> Interpret Value
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

evalExpr :: Expr -> Interpret Value
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
    ValuePrim p -> p as'
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

evalPrint :: Bool -> [Value] -> Interpret Value
evalPrint nl as = do
  strs <- traverse showValue as
  liftIO (putStr (concat strs))
  liftIO (if nl then putStrLn "" else pure ())
  pure ValueNull

-- operations between two values
evalBinop :: Binop -> Value -> Value -> Interpret Value
evalBinop BinopPlus x y = addOrAppend x y
evalBinop BinopMinus x y = binopCheck checkNumber ValueNumber (-) x y
evalBinop BinopExp x y = binopCheck checkNumber ValueNumber (**) x y
evalBinop BinopMult x y = binopCheck checkNumber ValueNumber (*) x y
evalBinop BinopDiv x y = binopCheck checkNumber ValueNumber (/) x y
evalBinop BinopMod x y = binopCheck checkInt (ValueNumber . fromIntegral) mod x y
evalBinop BinopLessThan x y = binopCheck checkKey ValueBool (<) x y
evalBinop BinopGreaterThan x y = binopCheck checkKey ValueBool (>) x y
evalBinop BinopGreaterThanEq x y = binopCheck checkKey ValueBool (>=) x y
evalBinop BinopLessThanEq x y = binopCheck checkKey ValueBool (<=) x y
evalBinop BinopEq x y = fmap ValueBool (deepEquals x y)
evalBinop BinopNotEq x y = fmap (ValueBool . not) (deepEquals x y)
evalBinop BinopAnd x y = binopCheck checkBool ValueBool (&&) x y
evalBinop BinopOr x y = binopCheck checkBool ValueBool (||) x y

deepEquals :: Value -> Value -> Interpret Bool
deepEquals (ValueNumber x) (ValueNumber y) = pure (x == y)
deepEquals (ValueBool x) (ValueBool y) = pure (x == y)
deepEquals (ValueString x) (ValueString y) = pure (x == y)
deepEquals ValueNull ValueNull = pure True
deepEquals (ValueRef x) (ValueRef y) = do
  x' <- liftIO (readIORef x)
  y' <- liftIO (readIORef y)
  objEquals x' y'
  where
    objEquals (ObjectArray ax) (ObjectArray ay) =
      fmap and (traverse (uncurry deepEquals) (zip ax ay))
    objEquals (ObjectDict dx) (ObjectDict dy) =
      let eq ((k1, v1), (k2, v2)) = fmap (k1 == k2 &&) (deepEquals v1 v2)
          pairs = zip (M.toAscList dx) (M.toAscList dy)
       in fmap and (traverse eq pairs)
    objEquals _ _ = throwError ErrEq
deepEquals _ _ = throwError ErrEq

addOrAppend :: Value -> Value -> Interpret Value
addOrAppend (ValueNumber x) (ValueNumber y) = pure (ValueNumber (x + y))
addOrAppend (ValueString x) (ValueString y) = pure (ValueString (x ++ y))
addOrAppend (ValueRef x) (ValueRef y) = do
  x' <- liftIO (readIORef x)
  y' <- liftIO (readIORef y)
  o <- objAppend x' y'
  r <- liftIO (newIORef o)
  pure (ValueRef r)
  where
    objAppend :: Object -> Object -> Interpret Object
    objAppend (ObjectArray ax) (ObjectArray ay) = pure (ObjectArray (ax ++ ay))
    objAppend (ObjectDict dx) (ObjectDict dy) = pure (ObjectDict (dx `M.union` dy))
    objAppend _ _ = throwError ErrAdd
addOrAppend _ _ = throwError ErrAdd

-- operations on one value
evalMonop :: Monop -> Value -> Interpret Value
evalMonop MonopNot x = monopCheck checkBool ValueBool not x
evalMonop MonopNeg x = monopCheck checkNumber ValueNumber negate x

-- | Given a "check" function, that turns a value into an "a" (and
-- potentially fails), a binary operation that takes two "a"s to a
-- "b", and a way to get a Value from a "b", create a type-checked
-- binary operation on Values.
binopCheck ::
  (Value -> Interpret a) ->
  (b -> Value) ->
  (a -> a -> b) ->
  Value ->
  Value ->
  Interpret Value
binopCheck check result op x y = do
  x' <- check x
  y' <- check y
  if valueType x /= valueType y
    then throwError (ErrType (valueType x) (valueType y))
    else pure ()
  pure (result (x' `op` y'))

monopCheck ::
  (Value -> Interpret a) ->
  (b -> Value) ->
  (a -> b) ->
  Value ->
  Interpret Value
monopCheck check result op x = do
  x' <- check x
  pure (result (op x'))

lookupEnv :: Ident -> Interpret Value
lookupEnv var = do
  result <- findEnv var
  case result of
    Just (_, val) -> pure val
    Nothing -> throwError (ErrLookup var)

modifyEnv :: Ident -> Value -> Interpret ()
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
findEnv :: Ident -> Interpret (Maybe (IORef Env, Value))
findEnv var = do
  ref <- ask
  Env locals parent <- liftIO (readIORef ref)
  case M.lookup var locals of
    Just val -> pure (Just (ref, val))
    Nothing -> case parent of
      Just ref' -> do
        local (const ref') (findEnv var)
      Nothing -> pure Nothing

showValue :: Value -> Interpret String
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

checkNumber :: Value -> Interpret Double
checkNumber (ValueNumber n) = pure n
checkNumber v = throwError (ErrType VTypeNumber (valueType v))

checkInt :: Value -> Interpret Int
checkInt x = do
  num <- checkNumber x
  if num /= fromIntegral (round num :: Int)
    then throwError ErrIndex
    else pure (round num)

checkBool :: Value -> Interpret Bool
checkBool (ValueBool n) = pure n
checkBool v = throwError (ErrType VTypeBool (valueType v))

checkRef :: Value -> Interpret (IORef Object)
checkRef (ValueRef r) = pure r
checkRef v = throwError (ErrType VTypeBool (valueType v))

-- | Make sure that the value is a key type.
checkKey :: Value -> Interpret Value
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
