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
    | ListU [Expr]
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

stringLiteral :: Parser Expr
stringLiteral = StringU . T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

charLiteral :: Parser Expr
charLiteral = choice [
    string "''" $> CharU Nothing
  , CharU . Just <$> between (char '\'') (char '\'') L.charLiteral
  ]

list :: Parser Expr
list = ListU <$> brackets (sepBy expr (symbol ","))

expr :: Parser Expr
expr = choice
  [ parens expr
  , stringLiteral
  , charLiteral
  , list
  ]

data Type
    = StringTag
    | CharTag
    | ListTag Type
    deriving (Read, Show, Eq)

checkType :: Type -> ExprT -> Either TypeException ()
checkType StringTag (String _) = Right ()
checkType tag (String _) = Left . TypeException $ "expected type " <> tshow tag <> " found String"
checkType CharTag (Char _) = Right ()
checkType tag (Char _) = Left . TypeException $ "expected type " <> tshow tag <> " found Char"
checkType (ListTag t) (List t' xs) = 
    if t /= t'
    then Left . TypeException $ "expected type List of" <> tshow t <> " found List of" <> tshow t'
    else mapM_ (checkType t) xs
checkType tag (List tag' _) = Left . TypeException $ "expected type " <> tshow tag <> " found list of" <> tshow tag'

inferType :: ExprT -> Either TypeException Type
inferType (String _) = Right StringTag
inferType (Char _) = Right CharTag
inferType (List tag xs) = mapM_ (checkType tag) xs >> pure (ListTag tag)

data ExprT
    = String Text
    | Char (Maybe Char)
    | List Type [ExprT]
    deriving (Read, Show, Eq)
    
typecheck :: Expr -> Either TypeException ExprT
typecheck (StringU str) = Right $ String str
typecheck (CharU mc) = Right $ Char mc
typecheck (ListU elems@(x : _)) = do
    expectedElemType <- inferType =<< typecheck x
    checked <- traverse typecheck elems
    let expected = List expectedElemType checked
    fmap (const expected) (checkType (ListTag expectedElemType) expected)
typecheck (ListU _) = Right $ List StringTag []

newtype TypeException
    = TypeException Text
    deriving (Eq, Show, Read)

instance Exception TypeException where
    displayException (TypeException msg) = T.unpack msg