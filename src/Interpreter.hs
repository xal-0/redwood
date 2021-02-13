module Interpreter where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

import Syntax

newtype Env = Env (M.Map Ident Value)
  deriving Show

data Value = ValueNumber Double
           | ValueClosure [Ident] Block
           | ValueNull
  deriving Show

data Error = ErrLookup Ident
           | ErrType
           | ErrArgs
  deriving Show

type Interpreter a = StateT Env (Except Error) a
