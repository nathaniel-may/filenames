module Exceptions where

import           CustomPrelude         -- import all
import qualified Data.Text             as T
import           Text.Megaparsec.Error (errorBundlePretty, ParseErrorBundle)
import           Types                 (Name(..), Type(..))

newtype ParseException
    = ParseException (ParseErrorBundle Text Void)
    deriving (Show, Eq)

data TypeException
    = EmptySourceFile
    | FormatNotFound
    | ListTypeMismatch Type Type
    | NoValueNamed Name
    | TypeMismatch Type (Maybe Type)
    | TopLevelNotAssignment Type
    deriving (Read, Show, Eq)

data RuntimeException
    = Boop100
    | Boop200
    deriving (Read, Show, Eq)

data CompilationException
    = ParseErr ParseException
    | TypeErr TypeException
    deriving (Show, Eq)

instance Display ParseException where
    display (ParseException bundle) = T.pack $ errorBundlePretty bundle

instance Display TypeException where
    display EmptySourceFile = "EmptySourceFile: Source file cannot be empty"
    display FormatNotFound = "FormatNotFound: No value named 'format' found. 'format' is a required value."
    display (ListTypeMismatch expected got) = "ListTypeMismatch: List of type " <> display (ListTag expected) <> " but got " <> display (ListTag got) <> "." 
    display (NoValueNamed (Name name)) = "NoValueNamed: No value named '" <> name <> "'."
    display (TypeMismatch expected Nothing) = "TypeMismatch: Expected type " <> display expected <> " but got an uninferable type."
    display (TypeMismatch expected (Just got)) = "TypeMismatch: Expected type " <> display expected <> " but got type " <> display got <> "."
    display (TopLevelNotAssignment got) = "TopLevelNotAssignment: All top-level expressions must be assignments. Found the following types: " <> display got <> "."

instance Display RuntimeException where
    display Boop100 = "RuntimeException"
    display Boop200 = "RuntimeException"

instance Display CompilationException where
    display (ParseErr e)    = display e
    display (TypeErr e)     = display e
