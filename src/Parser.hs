module Parser where

import Control.Monad
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
        try stmtFuncDef,
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
    *> (StmtAssign <$> identifier <*> (ExprFunc <$> funcArgs <*> stmtBlock))

funcArgs :: Parser [Ident]
funcArgs = parens (identifier `sepBy` symbol ",")

stmtBlock :: Parser Block
stmtBlock = braces stmts
  where
    braces =
      between
        (symbol "{" *> many (lexeme newline))
        (symbol "}")

stmts :: Parser Block
stmts = stmt `sepEndBy` some (lexeme newline)

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
      args <- parens (expr `sepBy` symbol ",")
      pure (`ExprCall` args)

term :: Parser Expr
term =
  lexeme $
    choice
      [ number,
        exprFunc,
        variable,
        parens expr
      ]

exprFunc :: Parser Expr
exprFunc =
  symbol "func" *> (ExprFunc <$> funcArgs <*> stmtBlock)

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
    let s = first : end
    guard (s `notElem` reserved)
    pure s
  where
    identChar = letterChar <|> char '_'
    reserved =
      [ "func",
        "if",
        "while",
        "in",
        "for",
        "return",
        "else",
        "null",
        "true",
        "false"
      ]

sc :: Parser ()
sc = L.space hspace1 empty empty

symbol :: String -> Parser String
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
