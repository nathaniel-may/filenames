module Parsers where

import           CustomPrelude              hiding (many, some)
import qualified Data.Text                  as T
import           Exceptions                 (ParseException(..))      
import qualified Text.Megaparsec            as Mega
import           Text.Megaparsec            -- import all
import           Text.Megaparsec.Char       (char, digitChar, lowerChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Read                  (read)
import           Types                      (ExprU(..), Name(..))


type Parser = Parsec Void Text

parse :: Text -> Either ParseException ExprU
parse input = mapLeft ParseException $ BodyU <$> Mega.parse (some expr) "" input

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

curlies :: Parser a -> Parser a
curlies = between (symbol "{") (symbol "}")

stringLiteral :: Parser ExprU
stringLiteral = StringU . T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

list :: Parser ExprU
list = ListU <$> brackets (sepBy expr (symbol ","))

intLiteral :: Parser ExprU
intLiteral = IntU . read <$> some digitChar

boolLiteral :: Parser ExprU
boolLiteral = (\x -> if x == "true" then BoolU True else BoolU False) <$> (symbol "true" <|> symbol "false")

opIdentifierChar :: Parser Char
opIdentifierChar = satisfy (`elem` ['!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '|', '^', '-', '~'])

opIdentifier' :: Parser Text
opIdentifier' = T.pack <$> some opIdentifierChar

opIdentifier :: Parser ExprU
opIdentifier = InfixIdentifierU . Name <$> opIdentifier'

identifierChar :: Parser Char
identifierChar = satisfy (\x -> isDigit x || isAlpha x || x == '\''|| x == '_')

identifier' :: Parser Text
identifier' = T.pack <$> ((:) <$> lowerChar <*> many identifierChar) <* sc

identifier :: Parser ExprU
identifier = notFollowedBy (symbol "let") *> (IdentifierU . Name <$> identifier')

assignment :: Parser ExprU
assignment = AssignmentU . Name <$> (symbol "let" *> lexeme identifier') <* symbol ":=" <*> lexeme expr

apply :: Parser ExprU
apply = curlies (apply' =<< some (lexeme value)) where
  apply' [] = fail "cannot have an empty function block"
  apply' [_] = fail "unexpected value in a function block. try using parentheses instead of curlies."
  apply' (x : y : z) = pure $ foldr (flip ApplyU) (ApplyU x y) z

value :: Parser ExprU
value = choice
  [ stringLiteral
  , intLiteral
  , boolLiteral
  , list
  , opIdentifier
  , identifier
  ]

expr :: Parser ExprU
expr = choice
  [ parens expr
  , apply
  , value
  , assignment
  ]
