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

runParse :: Text -> Text -> Either CompileException Expr
runParse filename input = mapLeft ParseException (parse expr (T.unpack filename) input)

data Type
    = StringTag
    | CharTag
    | ListTag Type
    deriving (Read, Show, Eq)

checkType :: Type -> ExprT -> Either CompileException ()
checkType StringTag (String _) = Right ()
checkType tag (String _) = Left $ TypeException tag StringTag
checkType CharTag (Char _) = Right ()
checkType tag (Char _) = Left $ TypeException tag CharTag
checkType (ListTag t) (List t' xs) = 
    if t /= t'
    then Left $ TypeException (ListTag t) (ListTag t')
    else mapM_ (checkType t) xs
checkType t (List t' _) = Left $ TypeException (ListTag t) (ListTag t')

inferType :: ExprT -> Either CompileException Type
inferType (String _) = Right StringTag
inferType (Char _) = Right CharTag
inferType (List tag xs) = mapM_ (checkType tag) xs >> pure (ListTag tag)

data ExprT
    = String Text
    | Char (Maybe Char)
    | List Type [ExprT]
    deriving (Read, Show, Eq)
    
typecheck :: Expr -> Either CompileException ExprT
typecheck (StringU str) = Right $ String str
typecheck (CharU mc) = Right $ Char mc
typecheck (ListU elems@(x : _)) = do
    expectedElemType <- inferType =<< typecheck x
    checked <- traverse typecheck elems
    let possibility = List expectedElemType checked
    fmap (const possibility) (checkType (ListTag expectedElemType) possibility)
typecheck (ListU _) = Right $ List StringTag []

data CompileException 
    = ParseException (ParseErrorBundle Text Void)
    | TypeException { 
        expectedType :: Type
      , foundType    :: Type
    }
    deriving (Eq, Show)

instance Exception CompileException where
    displayException (ParseException e) =
        errorBundlePretty e
    displayException (TypeException e f) = 
        "expected " <> show e <> " but found " <> show f
 