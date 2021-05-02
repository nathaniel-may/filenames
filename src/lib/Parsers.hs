{-# LANGUAGE GADTs #-}

module Parsers where

import           Control.Exception              (displayException, Exception)
import           CustomPrelude                  hiding (count, some, many, Any)
import qualified Data.Text                      as T
import           Text.Megaparsec                -- import all
import           Text.Megaparsec.Char           (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer     as L

type Parser = Parsec Void Text

data Expr
    = StringU Text
    | CharU (Maybe Char)
    deriving (Read, Show, Eq)

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

-- brackets :: Parser a -> Parser a
-- brackets = between (symbol "[") (symbol "]")

stringLiteral :: Parser Expr
stringLiteral = StringU . T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

charLiteral :: Parser Expr
charLiteral = choice [
    string "''" $> CharU Nothing
  , CharU . Just <$> between (char '\'') (char '\'') L.charLiteral
  ]

expr :: Parser Expr
expr = choice
  [ parens expr
  , stringLiteral
  , charLiteral
  ]

data ExprT t where
    String :: Text -> ExprT Text
    Char   :: Maybe Char -> ExprT (Maybe Char)

-- extential type
data SomeAST = forall t. SomeAST (ExprT t)
    
typecheck :: Expr -> Either TypeException SomeAST
typecheck (StringU str) = Right . SomeAST $ String str
typecheck (CharU mc) = Right . SomeAST $ Char mc


newtype TypeException
    = TypeException Text
    deriving (Eq, Show, Read)

instance Exception TypeException where
    displayException (TypeException msg) = T.unpack msg