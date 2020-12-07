
module Filenames where

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

pTag :: [Text] -> Parser Tok
pTag schema = Tag <$> choice (string <$> schema)

pId :: Parser Tok
pId = Id . T.pack <$> count 6 (satisfy isIdChar) where 
    isIdChar '0' = False
    isIdChar c   = isAsciiUpper c || isDigit c

pCount :: Parser Tok
pCount = Count <$> L.decimal

pFilename :: [Text] -> Parser [Tok]
pFilename schema = do
    tags  <- some $ pTag schema <* char '-'
    setId <- pId
    i     <- optional (char '-' *> pCount)
    _     <- char '.' *> some alphaNumChar *> eof
    pure $ tags <> [setId] <> toList i