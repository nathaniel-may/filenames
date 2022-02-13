{-# LANGUAGE GADTs #-}

module Parsers where

import           CustomPrelude                  hiding (count, some, many, Any)
import qualified Data.Text                      as T
import           Text.Megaparsec                -- import all
import           Text.Megaparsec.Char           (alphaNumChar, char, space1, string)
import qualified Text.Megaparsec.Char.Lexer     as L

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

-- Config File Parsers

data Expr
    = Int Int
    | StrList [Text]
    | Fn Text [Expr]
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

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

int :: Parser Expr
int = Int <$> lexeme L.decimal

strlist :: Parser Expr
strlist = StrList <$> brackets (sepBy commalessStr (symbol ","))
    where commalessStr = T.pack <$> some (noneOf [',',']'])

fn :: Parser Expr
fn = do
    name <- (T.pack <$> many alphaNumChar) <* symbol "("
    params <- sepBy expr (symbol ",") <* symbol ")"
    pure (Fn name params)

term :: Parser Expr
term = choice
  [ parens expr
  , int 
  , strlist
  , fn ]

expr :: Parser Expr
expr = term -- TODO this is pretty simplistic

data ExprT t where
    UnitVal  :: ExprT ()
    IntT     :: Int       -> ExprT Int
    StrListT :: [Text]    -> ExprT [Text]
    ExactlyT :: ExprT Int -> ExprT [Text] -> ExprT ()
    AtLeastT :: ExprT Int -> ExprT [Text] -> ExprT ()

-- extential type
data SomeAST = forall t. SomeAST (ExprT t)
    
typecheck :: Expr -> Either TypeException SomeAST
typecheck (Int i) = Right . SomeAST $ IntT i
typecheck (StrList strs) = Right . SomeAST  $ StrListT strs
typecheck (Fn "exactly" args) = case args of
    [Int i, StrList strs] -> Right . SomeAST  $ ExactlyT (IntT i) (StrListT strs)
    _ -> Left . TypeException $ "`exactly` takes two args of type Int and StringList."
typecheck (Fn "atLeast" [arg0, arg1]) = case (arg0, arg1) of
    (Int i, StrList strs) -> Right . SomeAST  $ AtLeastT (IntT i) (StrListT strs)
    _ -> Left . TypeException $ "`atLeast` takes two args of type Int and StringList."
typecheck (Fn "one" [arg0]) = case arg0 of
    (StrList strs) -> Right . SomeAST  $ ExactlyT (IntT 1) (StrListT strs)
    _ -> Left . TypeException $ "`one` takes one arg of type StringList."
typecheck (Fn "any" [arg0]) = case arg0 of
    (StrList strs) -> Right . SomeAST  $ AtLeastT (IntT 0) (StrListT strs)
    _ -> Left . TypeException $ "`any` takes one arg of type StringList."
typecheck (Fn "anyExcept" [arg0]) = case arg0 of
    (StrList strs) -> Right . SomeAST  $ ExactlyT (IntT 0) (StrListT strs)
    _ -> Left . TypeException $ "`anyExcept` takes one arg of type StringList."
typecheck (Fn name _) = Left . TypeException $ "unknown function `" <> name <> "`"

newtype TypeException
    = TypeException Text
    deriving (Eq, Show, Read)

instance Exception TypeException where
    displayException (TypeException msg) = T.unpack msg