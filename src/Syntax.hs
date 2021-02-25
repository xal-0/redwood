module Syntax where

type Program = Block

type Ident = String

-- | A list of statements to be executed one after the other, say, as
-- the body of a function or a while loop.  Typically the value
-- resulting from evaluating this is the value that results from
-- evaluating the final statement.
type Block = [Stmt]

-- | Statements make up function bodies.  They evaluate to a value,
-- but this is null in the case of assignments.  Expressions are
-- automatically statements, but statements cannot be used in
-- expressions.
data Stmt
  = StmtExpr Expr
  | StmtAssign Ident Expr
  | StmtReturn (Maybe Expr)
  | StmtWhile Expr Block
  deriving (Show)

-- | Binary operations, like arithmetic.
data Binop = BinopPlus
  deriving (Show)

-- | Expressions, which evaluate to a value and can have side effects.
data Expr
  = -- | A reference to a variable in the current scope.
    ExprVariable Ident
  | -- | A literal number.
    ExprNumber Double
  | -- | A boolean.
    ExprBool Bool
  | -- | A chain of if, else if, and else statements
    ExprIfElseChain [(Expr, Block)] (Maybe Block)
  | -- | A binary operation (arithmetic).
    ExprBinop Binop Expr Expr
  | -- | Call a function (really just an expression that evaluates to
    -- a closure) with some arguments.
    ExprCall Expr [Expr]
  | -- | A lambda: the ident list are the formal parameters, and the
    -- block is the function body.
    ExprFunc [Ident] Block
  | -- | A literal array.
    ExprArray [Expr]
  deriving (Show)
