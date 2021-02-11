module Parser where

import Control.Monad.Combinators.Expr
import Data.Void
import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

symbol :: String -> Parser String
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = lexeme $ do
  first <- identChar
  end <- many (identChar <|> numberChar)
  pure (first : end)
  where
    identChar = letterChar <|> char '_'

expr :: Parser Expr
expr = makeExprParser term ops
  where
    ops =
      [ [ Postfix manyCall
        ],
        [ binary "+" BinopPlus
        ]
      ]

    manyCall = foldr1 (.) <$> some call

    binary name op = InfixL (ExprBinop op <$ symbol name)
    call = do
      args <- argList
      pure (\f -> ExprCall f args)

term :: Parser Expr
term =
  lexeme $
    choice
      [ number,
        variable,
        parens expr
      ]

argList :: Parser [Expr]
argList = parens (expr `sepBy` char ',')

call :: Parser Expr
call = ExprCall <$> expr <*> argList

variable :: Parser Expr
variable = ExprVariable <$> identifier

number :: Parser Expr
number = ExprNumber <$> L.signed sc (try float <|> integer)
  where
    integer = fromIntegral <$> L.decimal
    float = L.float
