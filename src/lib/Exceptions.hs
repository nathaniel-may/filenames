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
    | NoFunctionNamed Name
    | NotAFunction Name Type
    | TypeMismatch Type Type
    | TopLevelNotAssignment Type
    | TypeMismatchFnReturn Name Type Type
    | TypeMismatchFnParam Name Type Type
    | TypeMismatchNumFnParams Name Int Int
    | CannotApplyNotAFunction Type Type
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
    display (NoFunctionNamed (Name name)) = "NoFunctionNamed: No value named '" <> name <> "'."
    display (TypeMismatch expected got) = "TypeMismatch: Expected type " <> display expected <> " but got type " <> display got <> "."
    display (TopLevelNotAssignment got) = "TopLevelNotAssignment: All top-level expressions must be assignments. Found the following types: " <> display got <> "."
    display (TypeMismatchFnReturn (Name name) expected got) = "TypeMismatchFnReturn: Expected type " <> display expected <> ", but function '" <> name <> "' returns type " <> display got <> "."
    display (TypeMismatchFnParam (Name name) expected got) = "TypeMismatchFnParam: The function " <> name <> " expected parameter of type " <> display expected <> ", but got type " <> display got <> "."
    display (TypeMismatchNumFnParams (Name name) expected got) = "TypeMismatchNumFnParams: Function '" <> name <> "' requires " <> tshow expected <> "parameters. Got " <> tshow got <> "."
    display (NotAFunction (Name name) got) = "NotAFunction: Expected value " <> name <> " to be a function. Instead it's defined as a " <> display got <> "."
    display (CannotApplyNotAFunction nonfn applied) = "CannotApplyNotAFunction: value of type " <> display nonfn <> " is not a function. Cannot apply a value of type " <> display applied <> " to it."

instance Display RuntimeException where
    display Boop100 = "RuntimeException"
    display Boop200 = "RuntimeException"

instance Display CompilationException where
    display (ParseErr e)    = display e
    display (TypeErr e)     = display e
