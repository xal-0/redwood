module Parser (parseBlock) where

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Void
import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- | Opens a file and uses Megaparsec to parse the file for the
-- interpreter.
parseBlock :: String -> IO (Maybe Block)
parseBlock path = do
  contents <- readFile path
  case parse (sc' *> stmts <* eof) path contents of
    Left err -> do
      putStrLn (errorBundlePretty err)
      pure Nothing
    Right prog -> pure (Just prog)

-- | Parser for statements.  This can be a plain expression, if we are
-- executing the expression for the side effects.
stmt :: Parser Stmt
stmt =
  label "statement" $
    choice
      [ stmtReturn,
        try stmtFuncDef,
        try stmtWhile,
        try stmtFor,
        try stmtAssignment,
        StmtExpr <$> expr
      ]

-- | Matches an assignment, which is composed of and expression, "=",
-- and another expression
stmtAssignment :: Parser Stmt
stmtAssignment =
  StmtAssign <$> expr
    <*> (symbol "=" *> expr)

-- | Matches the retern expression, which may be followed by an expression to be returned.
stmtReturn :: Parser Stmt
stmtReturn = symbol "return" *> (StmtReturn <$> optional expr)

-- | Matches a while loop.
stmtWhile :: Parser Stmt
stmtWhile =
  symbol "while"
    *> (StmtWhile <$> expr <*> stmtBlock)

-- | Matches an enhanced for loop, similar in syntax to python for
-- loops.  For loops can have either a single binder, where it will
-- iterate over array items or dictionary keys, or two binders, where
-- it will iterate over key-value pairs.
stmtFor :: Parser Stmt
stmtFor = do
  index <- symbol "for" *> identifier
  value <- optional (symbol "," *> identifier)
  collection <- symbol "in" *> expr
  body <- stmtBlock
  pure (StmtFor index value collection body)

-- | Matches a function definition (keyword, arguments, function
-- body).
stmtFuncDef :: Parser Stmt
stmtFuncDef =
  symbol "func"
    *> (StmtFunc <$> identifier <*> funcArgs <*> stmtBlock)

-- | Parse a list of function arguments (parenthesis and
-- comma-separate list of parameters).
funcArgs :: Parser [Ident]
funcArgs = parens (identifier `sepBy` symbol ",")

-- | A block is a section of code surrounded by braces, for example
-- the body of a function.  Normally statements are separated by
-- newlines, but newlines before and after blocks are allowed.
stmtBlock :: Parser Block
stmtBlock = braces stmts
  where
    braces =
      between
        (symbol "{" *> sc')
        (symbol "}")

-- | A list of newline-separate statements.
stmts :: Parser Block
stmts = stmt `sepEndBy` some (lexeme newline)

-- | Parses an expression.
expr :: Parser Expr
expr = label "expression" $ makeExprParser term ops
  where
    -- | The operator table.  Listed in terms of decreasing
    -- precedence.
    ops =
      [ [manyCall (call <|> index <|> field)],
        [ prefix "-" MonopNeg,
          prefix "!" MonopNot
        ],
        [binary "**" BinopExp],
        [ binary "*" BinopMult,
          binary "/" BinopDiv,
          binary "%" BinopMod
        ],
        [ binary "+" BinopPlus,
          binary "-" BinopMinus
        ],
        [ binary "<=" BinopLessThanEq,
          binary ">=" BinopGreaterThanEq,
          binary "<" BinopLessThan,
          binary ">" BinopGreaterThan
        ],
        [ binary "==" BinopEq,
          binary "!=" BinopNotEq
        ],
        [binary "&&" BinopAnd],
        [binary "||" BinopOr]
      ]

    -- | From the megaparsec manual.  Parse many "call-like" things
    -- after an expression (call parenthesis, array indexing, field
    -- access), and then apply them all in reverse, so that the
    -- innermost one is on the left.
    manyCall p = Postfix (foldr1 (.) . reverse <$> some p)

    binary name op = InfixL (label "operator" $ ExprBinop op <$ symbol name)
    prefix name op = Prefix (label "operator" $ ExprMonop op <$ symbol name)

    -- | Parse a call: an expression followed by a list of
    -- comma-separated arguments in pretenses.
    call = do
      args <- parens (expr `sepBy` symbol ",")
      pure (`ExprCall` args)

    -- | Parse an array or dictionary index, an expression followed by
    -- an expression in square brackets.
    index = do
      symbol "["
      i <- expr
      symbol "]"
      pure (`ExprIndex` i)

    -- | Parse a field projection: an expression followed by a dot,
    -- followed by a field name (not an expression).
    field = do
      symbol "."
      fieldName <- identifier
      pure (`ExprIndex` ExprString fieldName)

-- | A term is the simplest form of an expression: not formed by using
-- binary, prefix, or postfix operators.  (It may contain operrators,
-- for example the parenthesis expression.)
term :: Parser Expr
term =
  lexeme $
    choice
      [ number,
        boolean,
        exprFunc,
        str,
        array,
        dictionary,
        ifElseChain,
        ExprNull <$ symbol "null",
        variable,
        parens expr
      ]

-- | Matches an if statement followed by any number of else if clauses
-- and a possible else.
ifElseChain :: Parser Expr
ifElseChain = do
  ifClause <- symbol "if" *> clause
  elseIfClauses <- many (try (symbol "else" *> symbol "if") *> clause)
  elseClause <- optional (symbol "else" *> stmtBlock)
  pure (ExprIfElseChain (ifClause : elseIfClauses) elseClause)
  where
    clause = (,) <$> expr <*> stmtBlock

-- | Parse an anonymous function (a lambda).  The syntax is the same
-- as a function definition, without a name.
exprFunc :: Parser Expr
exprFunc =
  symbol "func" *> (ExprFunc <$> funcArgs <*> stmtBlock)

-- | Parse a variable reference.
variable :: Parser Expr
variable = ExprVariable <$> identifier

-- | Matches an expression that can be interpreted as a single number,
-- like 5 or 3.2.
number :: Parser Expr
number =
  label "number" $
    ExprNumber <$> L.signed sc (try float <|> integer)
  where
    integer = fromIntegral <$> (L.decimal :: Parser Int)
    float = L.float

-- | Parse true or false.
boolean :: Parser Expr
boolean =
  label "boolean" $ ExprBool <$> (True <$ symbol "true" <|> False <$ symbol "false")

-- | Parse a string literal, with possible escape sequences.
str :: Parser Expr
str =
  label "string" $
    ExprString <$> (char '"' >> manyTill L.charLiteral (char '"'))

-- | Matches an array expression, like [1, 3, 5].
array :: Parser Expr
array = label "array" $ do
  symbol "[" <* sc'
  a <- expr `sepBy` commaNewline
  void sc'
  symbol "]"
  pure (ExprArray a)

-- | Matches an dictionary expression in the form {key: value, cool:
-- beans}.  Keys are identifiers in this syntax: they cannot be
-- expressions.
dictionary :: Parser Expr
dictionary = label "dictionary" $ do
  symbol "{" <* sc'
  entries <- entry `sepBy` commaNewline
  void sc'
  symbol "}"
  pure (ExprDict entries)
  where
    entry = (,) <$> (key <* symbol ":") <*> expr
    key = ExprString <$> identifier <|> expr

-- | Parse a comma, optionally followed by newlines and other
-- whitespace.
commaNewline :: Parser ()
commaNewline = void (symbol "," *> sc')

-- | Matches the name of a function or variable.
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

-- | The usual space consumer, consuming comments and only horizontal
-- whitespace (since we are newline-sensitive).
sc :: Parser ()
sc = L.space hspace1 lineComment empty

-- | A space consumer that also accepts newlines.
sc' :: Parser ()
sc' = L.space space1 lineComment empty

-- | Parse a comment that starts with # and goes to the end of the
-- line.
lineComment :: Parser ()
lineComment = L.skipLineComment "#"

-- | The symbol combinator matches the given string, followed by
-- optional horizontal whitespace.
symbol :: String -> Parser ()
symbol s = void (L.symbol sc s)

-- | Execute the given parser, followed by optional horizontal
-- whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Execute the given parser, but match between parenthesis.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
