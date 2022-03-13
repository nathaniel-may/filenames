module Parsers where

import           CustomPrelude              -- import all
import           Data.Text                  as T
import           Exceptions                 (ParseException(..))
import qualified Text.Megaparsec            as Mega
import           Text.Megaparsec            -- import all
import           Text.Megaparsec.Char       (char, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import           Types                      (ExprU(..))


type Parser = Parsec Void Text

parse :: Text -> Either ParseException ExprU
parse input = mapLeft (const Boop10) (Mega.parse expr "" input)

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

stringLiteral :: Parser ExprU
stringLiteral = StringU . T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

list :: Parser ExprU
list = ListU <$> brackets (sepBy expr (symbol ","))

expr :: Parser ExprU
expr = choice
  [ parens expr
  , stringLiteral
--   , intLiteral  -- TODO make this
--   , boolLiteral -- TODO make this
  , list
  ]
