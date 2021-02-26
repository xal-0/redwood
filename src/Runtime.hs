module Runtime where

import Data.IORef
import qualified Data.Map as M
import Syntax
import Control.Monad.Reader
import Control.Monad.Except

newtype Interpreter = Interpreter (IORef Env)

type Interpret a = ReaderT (IORef Env) (ExceptT Error IO) a

data Env = Env (M.Map Ident Value) (Maybe (IORef Env))

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
    ValueClosure (IORef Env) [Ident] Block
  | ValueNull
  | -- | A reference to an object on the heap.  This is for variables
    -- | that refer to things that can be mutated, like arrays or
    -- | dictionaries.  When you pass a reference into a function, the
    -- | reference is copied, but the object that it points to is not.
    -- | Assigning to a dictionary/array access expression mutates the
    -- | object pointed to by the reference.
    ValueRef (IORef Object)
  | -- | A builtin function, like print.
    ValuePrim Prim

type Prim = [Value] -> Interpret Value

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

data VType
  = VTypeNumber
  | VTypeBool
  | VTypeString
  | VTypeClosure
  | VTypeNull
  | VTypePrim
  | VTypeArray
  | VTypeDict
  deriving (Show, Eq)

data Error
  = ErrLookup Ident
  | ErrMismatch VType VType
  | ErrType VType VType
  | ErrArgs Int Int
  | ErrAssign
  | ErrKey Value
  | ErrIndex Value Value
  | ErrMisc String
