
module Parsers where

import           CustomPrelude                  hiding (count, some, many, Any)
import qualified Data.Text                      as T
import           Text.Megaparsec                -- import all
import           Text.Megaparsec.Char           (alphaNumChar, char, space1, string)
import qualified Text.Megaparsec.Char.Lexer     as L


type Parser = Parsec Void Text

data Tok
    = Tag Text | Id Text | Count Int
    deriving (Eq, Show, Read)

tag :: [Text] -> Parser Tok
tag schema = Tag <$> choice (string <$> schema)

idTag :: Parser Tok
idTag = Id . T.pack <$> count 6 (satisfy isIdChar) where 
    isIdChar '0' = False
    isIdChar c   = isAsciiUpper c || isDigit c

counter :: Parser Tok
counter = Count <$> L.decimal

filename :: [Text] -> Parser [Tok]
filename schema = do
    tags  <- some $ tag schema <* char '-'
    setId <- idTag
    i     <- optional (char '-' *> counter)
    _     <- char '.' *> some alphaNumChar *> eof
    pure $ tags <> [setId] <> toList i

-- Config File Parsers

data Expr
    = Char Char
    | Int Int
    | StrList [Text]
    | String Text
    | Var Text
    -- functions
    | Fn Text [Expr]
    deriving (Read, Show, Eq)

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

int :: Parser Expr
int = Int <$> lexeme L.decimal

strlist :: Parser Expr
strlist = StrList <$> brackets (sepBy commalessStr (symbol ","))
    where commalessStr = T.pack <$> some (noneOf [',',']'])

fn :: Parser Expr
fn = do
    name <- (T.pack <$> many alphaNumChar) <* symbol "("
    params <- (sepBy expr (symbol ",")) <* symbol ")"
    pure (Fn name params)

term :: Parser Expr
term = choice
  [ parens expr
  , int 
  , strlist
  , fn ]

expr :: Parser Expr
expr = term -- TODO this is pretty simplistic