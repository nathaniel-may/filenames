
module Parsers where

import           Data.Char                 (isAsciiUpper, isDigit)
import           Data.Foldable             (toList)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Void                 (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


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