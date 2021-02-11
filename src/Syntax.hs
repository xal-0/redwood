module Syntax where

data Binop = BinopPlus
  deriving Show

type Ident = String

data Expr = ExprVariable Ident
          | ExprNumber Double
          | ExprBinop Binop Expr Expr
          | ExprCall Expr [Expr]
  deriving Show
