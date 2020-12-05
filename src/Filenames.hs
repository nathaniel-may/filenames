
module Filenames where

import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text

data Tok
    = Tag Text | Id Text | Count Int
    deriving (Eq, Show, Read)

pTag :: [Text] -> Parser Tok
pTag schema = do
    t <- choice $ string <$> schema
    pure (Tag t)

pId :: Parser Tok
pId = do
    fid <- count 6 (choice $ char <$> T.unpack "ABCDEFGHIJKLMNOPQRSTUVWXYZ123456789")
    pure . Id $ T.pack fid

pCount :: Parser Tok
pCount = do 
    i <- L.decimal :: Parser Int
    pure (Count i)

pFilename :: [Text] -> Parser [Tok]
pFilename schema = do
    tags  <- some $ (pTag schema) <* char '-'
    setId <- pId
    i     <- optional (char '-' *> pCount)
    _     <- eof 
    pure $ tags <> [setId] <> (toList i)
