
module Parsers where

import           CustomPrelude              hiding (some)
import qualified Data.Text                  as T
import           Text.Megaparsec            -- import all
import           Text.Megaparsec.Char       (alphaNumChar, char, string)
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