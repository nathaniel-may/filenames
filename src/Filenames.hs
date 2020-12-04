{-# Language OverloadedStrings #-}

module Filenames where

import Control.Applicative ((<|>))
import Data.Char (isDigit, isUpper, isAlpha)
import Data.Either (lefts, rights)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (decimal)


data Tok 
    = A | B | C | D | E | F | G | Id | Count Int
    deriving (Eq, Ord)

idChars :: Char -> Bool
idChars '0' = False
idChars c   = isDigit c || (isAlpha c && isUpper c)

lexId :: Text -> Maybe (Tok, Text)
lexId str = 
    if T.length str == 6 && T.all idChars str
    then Just (Id, str) 
    else Nothing

lexCount :: Text -> Maybe (Tok, Text)
lexCount str = do
    (i, extra) <- fromEither num
    if T.null extra
    then Just (Count i, str)
    else Nothing
    where 
        num = decimal str
        fromEither (Right x) = Just x
        fromEither (Left _) = Nothing

tokenize :: Text -> Either Text (Tok, Text)
tokenize str@"a" = Right (A, str)
tokenize str@"b" = Right (C, str)
tokenize str@"c" = Right (C, str)
tokenize str@"d" = Right (D, str)
tokenize str@"e" = Right (E, str)
tokenize str@"f" = Right (F, str)
tokenize str@"g" = Right (G, str)
tokenize str = 
    maybe (Left ("lexing error: " <> str <> " is not a valid tag, id or count")) Right (lexId str <|> lexCount str)

lex :: Text -> Either [Text] [(Tok, Text)]
lex str =
    if null errors
    then Right (rights results)
    else Left errors
    where
        results = tokenize <$> T.split (=='-') str
        errors = lefts results