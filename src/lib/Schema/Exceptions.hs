module Schema.Exceptions where

import           Control.Exception  (displayException, Exception)
import           CustomPrelude      -- import all
import           Schema.Types       -- import all
import Text.Megaparsec.Error        (ParseErrorBundle, errorBundlePretty) 


data CompileException 
    = ParseE ParseException
    | TypeE TypeException
    deriving (Eq, Show)

data ParseException
    = LexicalException (ParseErrorBundle Text Void)
    | EmptyChar
    deriving (Eq, Show)

data TypeException
    = TypeException { 
        expectedType :: Type
      , foundType    :: Type
    }
    deriving (Eq, Show)

instance Exception CompileException where
    displayException (ParseE e) = displayException e
    displayException (TypeE e)  = displayException e
 
instance Exception ParseException where
    displayException (LexicalException e) =
        errorBundlePretty e
    displayException EmptyChar = 
        "char cannot be empty"

instance Exception TypeException where
    displayException (TypeException e f) =
        "expected " <> show e <> " but found " <> show f
