module Parser where

import Control.Monad.Combinators.Expr
import Data.Void
import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

stmt :: Parser Stmt
stmt =
  label "statement" $
    choice
      [ stmtReturn,
        stmtFuncDef,
        try stmtAssignment,
        StmtExpr <$> expr
      ]

stmtAssignment :: Parser Stmt
stmtAssignment =
  StmtAssign <$> identifier
    <*> (symbol "=" *> expr)

stmtReturn :: Parser Stmt
stmtReturn = symbol "return" *> (StmtReturn <$> optional expr)

stmtFuncDef :: Parser Stmt
stmtFuncDef =
  symbol "func"
    *> (StmtFuncDef <$> identifier <*> args <*> stmtBlock)
  where
    args = parens (identifier `sepBy` char ',')

stmtBlock :: Parser Block
stmtBlock =
  between (symbol "{") (symbol "}") (sepEndBy stmt (some (lexeme newline)))

expr :: Parser Expr
expr = label "expression" $ makeExprParser term ops
  where
    ops =
      [ [ Postfix manyCall
        ],
        [ binary "+" BinopPlus
        ]
      ]

    manyCall = foldr1 (.) <$> some call

    binary name op = InfixL (label "operator" $ ExprBinop op <$ symbol name)
    call = do
      args <- parens (expr `sepBy` char ',')
      pure (`ExprCall` args)

term :: Parser Expr
term =
  lexeme $
    choice
      [ number,
        variable,
        parens expr
      ]

variable :: Parser Expr
variable = ExprVariable <$> identifier

number :: Parser Expr
number =
  label "number" $
    ExprNumber <$> L.signed sc (try float <|> integer)
  where
    integer = fromIntegral <$> L.decimal
    float = L.float

identifier :: Parser String
identifier = label "identifier" $
  lexeme $ do
    first <- identChar
    end <- many (identChar <|> numberChar)
    pure (first : end)
  where
    identChar = letterChar <|> char '_'

sc :: Parser ()
sc = L.space hspace1 empty empty

symbol :: String -> Parser String
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
