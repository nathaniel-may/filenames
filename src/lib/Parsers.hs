module Parsers where

import           CustomPrelude              hiding (many, some)
import qualified Data.Text                  as T
import           Exceptions                 (ParseException(..))      
import qualified Text.Megaparsec            as Mega
import           Text.Megaparsec            -- import all
import           Text.Megaparsec.Char       (char, lowerChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import           Types                      (ExprU(..), Name(..))


type Parser = Parsec Void Text

parse :: Text -> Either ParseException ExprU
parse input = mapLeft ParseException $ Mega.parse expr "" input

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
list = ListU <$> brackets (sepBy (lexeme expr) (symbol ","))

identifierChar :: Parser Char
identifierChar = satisfy (\x -> isDigit x || isAlpha x || x == '\''|| x == '_')

identifier' :: Parser Text
identifier' = T.pack <$> ((:) <$> lowerChar <*> many identifierChar) <* sc

identifier :: Parser ExprU
identifier = IdentifierU . Name <$> identifier'

apply :: Parser ExprU
apply = apply' =<< some (lexeme value) where
  apply' [] = fail "source definition cannot be empty"
  apply' [val] = pure val
  apply' (x : y : z) = pure $ foldr (flip ApplyU) (ApplyU x y) z

value :: Parser ExprU
value = choice
  [ stringLiteral
  , list
  , identifier
  ]

expr :: Parser ExprU
expr = choice
  [ parens expr
  , apply
  , value
  ]
