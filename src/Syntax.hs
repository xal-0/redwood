module Syntax where

type Program = Block

type Ident = String
type Block = [Stmt]

data Stmt = StmtExpr Expr
          | StmtAssign Ident Expr
          | StmtReturn (Maybe Expr)
          | StmtFuncDef Ident [Ident] Block
  deriving Show

data Binop = BinopPlus
  deriving Show

data Expr = ExprVariable Ident
          | ExprNumber Double
          | ExprBinop Binop Expr Expr
          | ExprCall Expr [Expr]
  deriving Show
