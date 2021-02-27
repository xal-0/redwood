module Parser (parseBlock) where

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Void
import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- Opens a file and uses Megaparsec to parse the file for the interpreter
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

-- matches an assignment, which is composed of and expression, "=", and another expression
stmtAssignment :: Parser Stmt
stmtAssignment =
  StmtAssign <$> expr
    <*> (symbol "=" *> expr)

-- matches the retern expression, which may be followed by an expression to be returned
stmtReturn :: Parser Stmt
stmtReturn = symbol "return" *> (StmtReturn <$> optional expr)

-- matches a while loop
stmtWhile :: Parser Stmt
stmtWhile =
  symbol "while"
    *> (StmtWhile <$> expr <*> stmtBlock)

-- matches an enhanced for loop, similar in syntax to python for loops
stmtFor :: Parser Stmt
stmtFor = do
  index <- symbol "for" *> identifier
  value <- optional (symbol "," *> identifier)
  collection <- symbol "in" *> expr
  body <- stmtBlock
  pure (StmtFor index value collection body)

-- matches a function definition
stmtFuncDef :: Parser Stmt
stmtFuncDef =
  symbol "func"
    *> (StmtFunc <$> identifier <*> funcArgs <*> stmtBlock)

funcArgs :: Parser [Ident]
funcArgs = parens (identifier `sepBy` symbol ",")

-- a block is a section of code surrounded by braces, for example the body of a function
stmtBlock :: Parser Block
stmtBlock = braces stmts
  where
    braces =
      between
        (symbol "{" *> sc')
        (symbol "}")

stmts :: Parser Block
stmts = stmt `sepEndBy` some (lexeme newline)

-- finds any operations between 2 operators
expr :: Parser Expr
expr = label "expression" $ makeExprParser term ops
  where
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

    -- From the megaparsec manual.  Parse many "call-like" things
    -- after an expression (call parenthesis, array indexing, field
    -- access), and then apply them all in reverse, so that the
    -- innermost one is on the left.
    manyCall p = Postfix (foldr1 (.) . reverse <$> some p)

    binary name op = InfixL (label "operator" $ ExprBinop op <$ symbol name)
    prefix name op = Prefix (label "operator" $ ExprMonop op <$ symbol name)

    call = do
      args <- parens (expr `sepBy` symbol ",")
      pure (`ExprCall` args)

    index = do
      symbol "["
      i <- expr
      symbol "]"
      pure (`ExprIndex` i)

    field = do
      symbol "."
      fieldName <- identifier
      pure (`ExprIndex` ExprString fieldName)

-- a term is the simplest form of an expression
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

-- matches an if statement followed by any number of else if clauses and a possible else
ifElseChain :: Parser Expr
ifElseChain = do
  ifClause <- symbol "if" *> clause
  elseIfClauses <- many (try (symbol "else" *> symbol "if") *> clause)
  elseClause <- optional (symbol "else" *> stmtBlock)
  pure (ExprIfElseChain (ifClause : elseIfClauses) elseClause)
  where
    clause = (,) <$> expr <*> stmtBlock

exprFunc :: Parser Expr
exprFunc =
  symbol "func" *> (ExprFunc <$> funcArgs <*> stmtBlock)

variable :: Parser Expr
variable = ExprVariable <$> identifier

-- matches an expression that can be interpreted as a single number, like 5 or 3.2
number :: Parser Expr
number =
  label "number" $
    ExprNumber <$> L.signed sc (try float <|> integer)
  where
    integer = fromIntegral <$> (L.decimal :: Parser Int)
    float = L.float

boolean :: Parser Expr
boolean =
  label "boolean" $ ExprBool <$> (True <$ symbol "true" <|> False <$ symbol "false")

str :: Parser Expr
str =
  label "string" $
    ExprString <$> (char '"' >> manyTill L.charLiteral (char '"'))

-- matches an array definition in the form [1, 3, 5]
array :: Parser Expr
array = label "array" $ do
  symbol "[" <* sc'
  a <- expr `sepBy` commaNewline
  void sc'
  symbol "]"
  pure (ExprArray a)

-- matches an dictionary definition in the form {"key":value, "cool":beans}
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

commaNewline :: Parser ()
commaNewline = void (symbol "," *> sc')

-- matches the name of a function or variable
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
sc = L.space hspace1 lineComment empty

sc' :: Parser ()
sc' = L.space space1 lineComment empty

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

symbol :: String -> Parser ()
symbol s = void (L.symbol sc s)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
