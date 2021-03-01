module Interpreter where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable
import Data.IORef
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Parser
import Runtime
import Syntax
import System.Random
import Utils

-- | Create a fresh interpreter with the default global environment.
makeInterpreter :: [(Ident, Prim)] -> IO Interpreter
makeInterpreter extraBuiltins = do
  let builtins =
        initialBuiltins
          ++ fmap
            (\(i, p) -> (i, ValuePrim p))
            (extraBuiltins ++ initialPrims)
      env = Env (M.fromList builtins) Nothing
  ref <- newIORef env
  pure (Interpreter ref)

-- | Helper function that executes an interpreter action (possibly
-- mutating the interpreter's state).  On success, returns the result.
-- On failure, pretty-print the error and return Nothing.
runInterpret :: Interpreter -> Interpret a -> IO (Maybe a)
runInterpret (Interpreter ref) int = do
  result <- runExceptT (runReaderT int ref)
  case result of
    Left (Left err) -> do
      Right str <- runExceptT (runReaderT (showError err) ref)
      putStrLn ("error: " ++ str)
      pure Nothing
    Left (Right _) -> do
      putStrLn "error: cannot return from toplevel code"
      pure Nothing
    Right a -> pure (Just a)

-- | Load and evaluate the source code at the path.  If there is an
-- error, pretty-print it and return False.
evalSource :: Interpreter -> FilePath -> IO Bool
evalSource interpreter path = do
  block <- parseBlock path
  case block of
    Nothing -> pure False
    Just b -> fmap isJust (runInterpret interpreter (evalBlock b))

-- | Call a function with no arguments, in an existing interpreter
-- environment.  The function must be defined (bound to a variable).
evalCall :: Interpreter -> String -> IO (Maybe Value)
evalCall interpreter var =
  let prog = StmtExpr (ExprCall (ExprVariable var) [])
   in runInterpret interpreter (evalStmt prog)

-- | A list of bindings to values that are put into every program when
-- it starts evaluating.
initialBuiltins :: [(Ident, Value)]
initialBuiltins =
  [ ("pi", ValueNumber pi),
    ("E", ValueNumber (exp 1))
  ]

-- | Mappings from variable names to built-in functions.  Programs get
-- these bindings in their environment when they start.
initialPrims :: [(Ident, Prim)]
initialPrims =
  [ ("println", evalPrint True),
    ("print", evalPrint False),
    ("push", evalPush),
    ("delete", evalDelete),
    ("string", evalString),
    ("length", evalLength),
    ("sin", evalMathPrim "sin" sin),
    ("cos", evalMathPrim "cos" cos),
    ("tan", evalMathPrim "tan" tan),
    ("asin", evalMathPrim "asin" asin),
    ("acos", evalMathPrim "acos" acos),
    ("atan", evalMathPrim "atan" atan),
    ("sqrt", evalMathPrim "sqrt" sqrt),
    ("log", evalMathPrim "log" log),
    ("rand", evalRand False),
    ("randi", evalRand True),
    ("abs", evalMathPrim "abs" abs),
    ("ceil", evalMathPrim "ceil" ((fromIntegral :: Int -> Double) . ceiling)),
    ("floor", evalMathPrim "floor" ((fromIntegral :: Int -> Double) . floor)),
    ("round", evalMathPrim "round" ((fromIntegral :: Int -> Double) . round))
  ]

-- | Given an environment, list of formal parameters, a function body,
-- and arguments to bind to those parameters, evaluate the function
-- body in the environment augmented with the parameters bound to the
-- arguments.
apply :: IORef Env -> [Ident] -> Block -> [Value] -> Interpret Value
apply env params body args
  | length params /= length args =
    throwRunError
      (ErrArgs (length params) (length args))
  | otherwise = do
    let argBinds = M.fromList (zip params args)
    env' <- liftIO (newIORef (Env argBinds (Just env)))
    local (const env') (evalBlock body)
      `catchError` handleReturn
  where
    handleReturn (Left e) = throwRunError e
    handleReturn (Right v) = pure v

-- | Given a list of variables, and values to bind to them, and a
-- block, evaluate the block in a fresh scope containing the bindings.
evalScope :: [(Ident, Value)] -> Block -> Interpret Value
evalScope binds block = do
  parent <- ask
  let env = Env (M.fromList binds) (Just parent)
  env' <- liftIO (newIORef env)
  local (const env') (evalBlock block)

-- | Evaluate a block in the current scope.
evalBlock :: Block -> Interpret Value
evalBlock [] = pure ValueNull
evalBlock (s@(StmtReturn _) : _) = evalStmt s
evalBlock [stmt] = evalStmt stmt
evalBlock (stmt : stmts) = evalStmt stmt >> evalBlock stmts

-- | Evaluate a statement in the current scope.
evalStmt :: Stmt -> Interpret Value
evalStmt s@(StmtWhile condition conditional) = do
  conditionValue <- evalExpr condition
  conditionBool <- checkBool conditionValue
  if conditionBool
    then evalScope [] conditional >> evalStmt s
    else pure ValueNull
evalStmt (StmtFor k v e b) = do
  e' <- evalExpr e
  evalFor k v e' b
  pure ValueNull
evalStmt (StmtExpr e) = evalExpr e
evalStmt (StmtFunc v ps body) =
  evalStmt (StmtAssign (ExprVariable v) (ExprFunc ps body))
evalStmt (StmtAssign (ExprVariable v) e) = do
  e' <- evalExpr e
  modifyEnv v e'
  pure ValueNull
evalStmt (StmtAssign (ExprIndex indexable index) e) = do
  indexable' <- evalExpr indexable
  ref' <- checkRef indexable'
  index' <- evalExpr index
  obj <- liftIO (readIORef ref')
  e' <- evalExpr e
  case obj of
    ObjectArray a -> do
      i <- checkInt index'
      case replaceIdx a i e' of
        Nothing -> throwRunError (ErrIndex indexable' index')
        Just a' -> do
          liftIO (writeIORef ref' (ObjectArray a'))
    ObjectDict d -> do
      _ <- checkKey index'
      let d' = M.insert index' e' d
      liftIO (writeIORef ref' (ObjectDict d'))
  pure ValueNull
evalStmt (StmtAssign _ _) = throwRunError ErrAssign
evalStmt (StmtReturn Nothing) = throwReturn ValueNull
evalStmt (StmtReturn (Just r)) = evalExpr r >>= throwReturn

-- | Evaluate a for loop.  Can take either one variable, or two.  With
-- one variable, it will iterate over the elements of an array, or the
-- keys of a dictionary.  With two variables, it will iterate over the
-- keys and values (respectively) of a dictionary.  Evaluates the loop
-- body in a new scope with the new bindings for every iteration.
evalFor :: Ident -> Maybe Ident -> Value -> Block -> Interpret ()
evalFor value Nothing (ValueRef r) body = do
  obj <- liftIO (readIORef r)
  case obj of
    ObjectArray a -> traverse_ (\v -> evalScope [(value, v)] body) a
    _ -> throwRunError (ErrType VTypeArray (objectType obj))
evalFor _ Nothing v _ = ErrType VTypeArray <$> valueType v >>= throwRunError
evalFor key (Just value) (ValueRef r) body = do
  obj <- liftIO (readIORef r)
  pairs <- case obj of
    ObjectArray a -> pure (zip (fmap ValueNumber [0 ..]) a)
    ObjectDict d -> pure (M.toList d)
  traverse_ (\(k, v) -> evalScope [(value, v), (key, k)] body) pairs
evalFor _ (Just _) v _ = ErrType VTypeDict <$> valueType v >>= throwRunError

-- | Evaluate an expression in the current scope.
evalExpr :: Expr -> Interpret Value
evalExpr (ExprVariable v) = lookupEnv v
evalExpr (ExprNumber n) = pure (ValueNumber n)
evalExpr (ExprString n) = pure (ValueString n)
evalExpr (ExprBool n) = pure (ValueBool n)
evalExpr ExprNull = pure ValueNull
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
    _ -> ErrType VTypeClosure <$> valueType f' >>= throwRunError
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
evalExpr (ExprIfElseChain [] (Just els)) = evalScope [] els
evalExpr (ExprIfElseChain ((cond, body) : xs) els) = do
  conditionValue <- evalExpr cond
  conditionBool <- checkBool conditionValue
  if conditionBool
    then evalScope [] body
    else evalExpr (ExprIfElseChain xs els)
evalExpr (ExprIndex x index) = do
  x' <- evalExpr x
  index' <- evalExpr index
  let err = maybe (throwRunError (ErrIndex x' index')) pure
  case x' of
    ValueString s -> do
      i <- checkInt index'
      c <- maybe (throwRunError (ErrIndex x' index')) pure (s !!? i)
      pure (ValueString [c])
    ValueRef ref -> do
      obj <- liftIO (readIORef ref)
      case obj of
        ObjectArray a -> do
          i <- checkInt index'
          maybe (throwRunError (ErrIndex x' index')) pure (a !!? i)
        ObjectDict d -> do
          _ <- checkKey index'
          err (M.lookup index' d)
    _ -> err Nothing

-- | Print the given value.  The first argument determines whether it
-- is terminated with a newline or not.
evalPrint :: Bool -> Prim
evalPrint nl as = do
  strs <- traverse showValue as
  liftIO (putStr (concat strs))
  liftIO (if nl then putStrLn "" else pure ())
  pure ValueNull

-- | Push a value to the end of an array, mutating it.
evalPush :: Prim
evalPush [ValueRef r, x] = do
  o <- liftIO (readIORef r)
  case o of
    ObjectArray a -> liftIO (writeIORef r (ObjectArray (a ++ [x])))
    _ -> throwRunError (ErrMismatch VTypeArray (objectType o))
  pure ValueNull
evalPush _ = throwRunError (ErrMisc "wrong arguments for push")

-- | Delete (mutating) the given index or key from an array or
-- dictionary, respectively.
evalDelete :: Prim
evalDelete [ValueRef r, x] = do
  o <- liftIO (readIORef r)
  case o of
    ObjectArray a -> do
      i <- checkInt x
      if i >= length a
        then throwRunError (ErrIndex (ValueRef r) x)
        else liftIO (writeIORef r (ObjectArray (take i a ++ drop (i + 1) a)))
    ObjectDict d -> do
      _ <- checkKey x
      if M.member x d
        then liftIO (writeIORef r (ObjectDict (M.delete x d)))
        else throwRunError (ErrIndex (ValueRef r) x)
  pure ValueNull
evalDelete _ = throwRunError (ErrMisc "wrong arguments for delete")

-- | Convert the given value to a string, in the same way that print
-- would show it.
evalString :: Prim
evalString [x] = ValueString <$> showValue x
evalString _ = throwRunError (ErrMisc "string takes one argument")

-- | Find the length of the given array, or the number of keys in a
-- dictionary.
evalLength :: Prim
evalLength [ValueString s] = pure (ValueNumber (fromIntegral (length s)))
evalLength [ValueRef r] = do
  o <- liftIO (readIORef r)
  case o of
    ObjectArray a -> pure (ValueNumber (fromIntegral (length a)))
    ObjectDict d -> pure (ValueNumber (fromIntegral (M.size d)))
evalLength _ = throwRunError (ErrMisc "length takes one argument")

-- | Evaluate a function (with the given name) on doubles.
evalMathPrim :: String -> (Double -> Double) -> Prim
evalMathPrim _ f [ValueNumber x] = pure (ValueNumber (f x))
evalMathPrim name _ _ = throwRunError (ErrMisc (name ++ "takes one number argument"))

-- | Generate a random number in the given range (low, then high
-- bound, inclusive).  If the first argument is False, generate any
-- float in the range.  If it is True, then generate an integer.
evalRand :: Bool -> Prim
evalRand False [ValueNumber l, ValueNumber h] = ValueNumber <$> liftIO (randomRIO (l, h))
evalRand True [l, h] = do
  l' <- checkInt l
  r' <- checkInt h
  ValueNumber . fromIntegral <$> liftIO (randomRIO (l', r'))
evalRand _ _ = throwRunError (ErrMisc "rand takes two arguments")

-- | Evaluate an operations between two values.
evalBinop :: Binop -> Value -> Value -> Interpret Value
evalBinop BinopPlus x y = addOrAppend x y
evalBinop BinopMinus x y = binopCheck checkNumber ValueNumber (-) x y
evalBinop BinopExp x y = binopCheck checkNumber ValueNumber (**) x y
evalBinop BinopMult x y = binopCheck checkNumber ValueNumber (*) x y
evalBinop BinopDiv x y = binopCheck checkNumber ValueNumber (/) x y
evalBinop BinopMod x y = binopCheck checkNumber ValueNumber fmod x y
evalBinop BinopLessThan x y = binopCheck checkKey ValueBool (<) x y
evalBinop BinopGreaterThan x y = binopCheck checkKey ValueBool (>) x y
evalBinop BinopGreaterThanEq x y = binopCheck checkKey ValueBool (>=) x y
evalBinop BinopLessThanEq x y = binopCheck checkKey ValueBool (<=) x y
evalBinop BinopEq x y = fmap ValueBool (deepEquals x y)
evalBinop BinopNotEq x y = fmap (ValueBool . not) (deepEquals x y)
evalBinop BinopAnd x y = binopCheck checkBool ValueBool (&&) x y
evalBinop BinopOr x y = binopCheck checkBool ValueBool (||) x y

-- | The floating-point equivalent to integer mod: return the float
-- such that floor(a / b) * b + a = fmod(a, b). From
-- https://stackoverflow.com/a/64163086. Weird omission from the
-- standard library.
fmod :: Double -> Double -> Double
fmod x y = x - y * fromIntegral (floor (x / y) :: Int)

-- | Compare two values, going through references (compare the
-- contents of arrays and dictionaries, recursively, instead of
-- relying on reference equality).
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
    objEquals o1 o2 = throwRunError (ErrType (objectType o1) (objectType o2))
deepEquals x y = ErrType <$> valueType x <*> valueType y >>= throwRunError

-- | If the two values are numbers, add them.  If they are arrays or
-- strings, append them (non-mutating).  If it is a dictionary,
-- compute the union, where keys in the left map override ones in the
-- right (non-mutating).
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
    objAppend o1 o2 = throwRunError (ErrType (objectType o1) (objectType o2))
addOrAppend x y = ErrType <$> valueType x <*> valueType y >>= throwRunError

-- | Operations on one value.
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
  notSameType <- (/=) <$> valueType x <*> valueType y
  if notSameType
    then ErrType <$> valueType x <*> valueType y >>= throwRunError
    else pure ()
  pure (result (x' `op` y'))

-- | Helper to typecheck one value, and execute an operation on it.
monopCheck ::
  (Value -> Interpret a) ->
  (b -> Value) ->
  (a -> b) ->
  Value ->
  Interpret Value
monopCheck check result op x = do
  x' <- check x
  pure (result (op x'))

-- | Do a lookup.  If the variable is in the current environment,
-- return it, else recurse upwards into outer scopes.
lookupEnv :: Ident -> Interpret Value
lookupEnv var = do
  result <- findEnv var
  case result of
    Just (_, val) -> pure val
    Nothing -> throwRunError (ErrLookup var)

-- | Mutate a variable, either in the current environment or an
-- enclosing one.
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
-- in, as well as the value it is bound to, and Nothing if it is not
-- defined in any of them.
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

-- | Convert a value to a string, for use in print.
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

-- | Helper to check if the value is a number and throw an error
-- otherwise.
checkNumber :: Value -> Interpret Double
checkNumber (ValueNumber n) = pure n
checkNumber v = ErrType VTypeNumber <$> valueType v >>= throwRunError

-- | Helper to check if the value is both a number and an integer (0
-- fractional part).
checkInt :: Value -> Interpret Int
checkInt x = do
  num <- checkNumber x
  if num /= fromIntegral (round num :: Int)
    then throwRunError (ErrMisc "expected an integer index")
    else pure (round num)

-- | Helper to check if the value is a boolean.
checkBool :: Value -> Interpret Bool
checkBool (ValueBool n) = pure n
checkBool v = ErrType VTypeBool <$> valueType v >>= throwRunError

-- | Helper to check if the value is a reference type (array or
-- dictionary).
checkRef :: Value -> Interpret (IORef Object)
checkRef (ValueRef r) = pure r
checkRef v = ErrType VTypeBool <$> valueType v >>= throwRunError

-- | Make sure that the value is a key type (immutable and
-- comparable): a number, string, null, or a boolean.
checkKey :: Value -> Interpret Value
checkKey v@(ValueNumber _) = pure v
checkKey v@(ValueString _) = pure v
checkKey v@(ValueBool _) = pure v
checkKey ValueNull = pure ValueNull
checkKey _ = throwRunError (ErrMisc "expected an (immutable) key type")

-- | Return the type code for the given value (for error codes, etc).
valueType :: Value -> Interpret VType
valueType (ValueNumber _) = pure VTypeNumber
valueType (ValueBool _) = pure VTypeBool
valueType (ValueString _) = pure VTypeString
valueType ValueClosure {} = pure VTypeClosure
valueType ValueNull = pure VTypeNull
valueType (ValuePrim _) = pure VTypePrim
valueType (ValueRef r) = do
  o <- liftIO (readIORef r)
  pure (objectType o)

-- | Return the type of an object (heap value).
objectType :: Object -> VType
objectType (ObjectArray _) = VTypeArray
objectType (ObjectDict _) = VTypeDict

-- | Pretty-print an error code.
showError :: Error -> Interpret String
showError (ErrLookup v) = pure ("unknown variable " ++ v)
showError (ErrMismatch t1 t2) = pure ("mismatched types: " ++ show t1 ++ " and " ++ show t2)
showError (ErrType t1 t2) = pure ("expected " ++ show t1 ++ ", got " ++ show t2)
showError (ErrArgs expected got) = pure ("expected " ++ show expected ++ " arguments, got " ++ show got)
showError ErrAssign = pure "malformed assignment (must be to a variable, index, or field)"
showError (ErrIndex x i) = do
  xs <- showValue x
  is <- showValue i
  pure ("index " ++ is ++ " does not exist in " ++ xs)
showError (ErrMisc misc) = pure misc

-- | Throw a runtime error (and not a returned value).
throwRunError :: Error -> Interpret a
throwRunError e = throwError (Left e)

-- | Start unwinding the interpreter, to do an early return.
throwReturn :: Value -> Interpret a
throwReturn v = throwError (Right v)
