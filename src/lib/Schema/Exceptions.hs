module Schema.Exceptions where

import           Control.Exception  (displayException, Exception)
import           CustomPrelude      -- import all
import           Schema.Types       -- import all
import Text.Megaparsec.Error        (ParseErrorBundle, errorBundlePretty) 


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
 