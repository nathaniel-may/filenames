{-# LANGUAGE GADTs #-}

module Parsers where

import           Control.Exception              (displayException, Exception)
import           CustomPrelude                  hiding (count, some, many, Any)
import qualified Data.Text                      as T
import           Text.Megaparsec                -- import all
import           Text.Megaparsec.Char           (char, space1)
import qualified Text.Megaparsec.Char.Lexer     as L

type Parser = Parsec Void Text

newtype Expr
    = StringU Text
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

expr :: Parser Expr
expr = choice
  [ parens expr
  , stringLiteral
  ]

data ExprT t where
    String  :: Text -> ExprT Text

-- extential type
data SomeAST = forall t. SomeAST (ExprT t)
    
typecheck :: Expr -> Either TypeException SomeAST
typecheck (StringU str) = Right . SomeAST $ String str

newtype TypeException
    = TypeException Text
    deriving (Eq, Show, Read)

instance Exception TypeException where
    displayException (TypeException msg) = T.unpack msg