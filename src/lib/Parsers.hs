module Parsers where

import           CustomPrelude              hiding (many, some)
import           Data.Text                  as T
import           Exceptions                 (ParseException(..))      
import qualified Text.Megaparsec            as Mega
import           Text.Megaparsec            -- import all
import           Text.Megaparsec.Char       (char, digitChar, lowerChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Read                  (read)
import           Types                      (ExprU(..), Name(..))


type Parser = Parsec Void Text

parse :: Text -> Either ParseException ExprU
parse input = mapLeft ParseException $ RootU <$> Mega.parse (some topExpr) "" input

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

intLiteral :: Parser ExprU
intLiteral = IntU . read <$> some digitChar

boolLiteral :: Parser ExprU
boolLiteral = (\x -> if x == "true" then BoolU True else BoolU False) <$> (symbol "true" <|> symbol "false")

identifierChar :: Parser Char
identifierChar = satisfy (\x -> isDigit x || isAlpha x || x == '\''|| x == '_')

identifier :: Parser ExprU
identifier = IdentifierU . Name .T.pack <$> ((:) <$> lowerChar <*> many identifierChar)

-- TODO handle whitespace better here?
assignment :: Parser ExprU
assignment = fromIdentifier <$> identifier <* sc <* symbol ":=" <*> expr where
    fromIdentifier (IdentifierU name) = AssignmentU name
    fromIdentifier _ = AssignmentU (Name "internal_error") -- TODO this is a hack. fix it.

expr :: Parser ExprU
expr = choice
  [ parens expr
  , stringLiteral
  , intLiteral
  , boolLiteral
  , list
  , identifier
  ]

topExpr :: Parser ExprU
topExpr = sc *> assignment <* sc
