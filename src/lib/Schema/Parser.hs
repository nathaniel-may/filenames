module Schema.Parser where

import           CustomPrelude                  hiding (count, some, many, Any)
import qualified Data.Text                      as T
import           Schema.Types                   -- import all
import           Schema.Exceptions              -- import all
import           Text.Megaparsec                -- import all
import           Text.Megaparsec.Char           (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer     as L


type Parser = Parsec Void Text

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

stringLiteral :: Parser Expr
stringLiteral = StringU . T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

charLiteral :: Parser Expr
charLiteral = choice [
    string "''" $> CharU Nothing
  , CharU . Just <$> between (char '\'') (char '\'') L.charLiteral
  ]

list :: Parser Expr
list = ListU <$> brackets (sepBy expr (symbol ","))

expr :: Parser Expr
expr = choice
  [ parens expr
  , stringLiteral
  , charLiteral
  , list
  ]

-- TODO make input record type to disambiguate Text inputs.
-- | top-level runner
runParse :: Text -> Text -> Either CompileException Expr
runParse filename input = mapLeft ParseException (parse expr (T.unpack filename) input)
