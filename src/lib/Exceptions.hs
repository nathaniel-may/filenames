module Exceptions where

import           CustomPrelude         -- import all
import qualified Data.Text             as T
import           Text.Megaparsec.Error (errorBundlePretty, ParseErrorBundle)
import           Types                 (Name(..), Type(..))

newtype ParseException
    = ParseException (ParseErrorBundle Text Void)
    deriving (Show, Eq)

data TypeException
    = EmptyBody
    | TopLevelMustBeSchema
    | ListTypeMismatch Type Type
    | NoValueNamed Name
    | TypeMismatch Type Type
    | TypeMismatchFnReturn Name Type Type
    | TypeMismatchFnParam Name Type Type
    | TypeMismatchNumFnParams Name Int Int
    | CannotApplyNotAFunction Type Type
    | BadPartialApplication Type
    | UnexpectedValue Type -- TODO kind of a cheap exception
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
    display EmptyBody = "EmptyBody: Body of a closure cannot be empty"
    display TopLevelMustBeSchema = "The top-level value must be a Schema"
    display (ListTypeMismatch expected got) = "ListTypeMismatch: List of type " <> display (ListTag expected) <> " but got " <> display (ListTag got) <> "." 
    display (NoValueNamed name) = "NoValueNamed: No value named '" <> display name <> "'."
    display (TypeMismatch expected got) = "TypeMismatch: Expected type " <> display expected <> " but got type " <> display got <> "."
    display (TypeMismatchFnReturn name expected got) = "TypeMismatchFnReturn: Expected type " <> display expected <> ", but function '" <> display name <> "' returns type " <> display got <> "."
    display (TypeMismatchFnParam name expected got) = "TypeMismatchFnParam: The function " <> display name <> " expected parameter of type " <> display expected <> ", but got type " <> display got <> "."
    display (TypeMismatchNumFnParams name expected got) = "TypeMismatchNumFnParams: Function '" <> display name <> "' requires " <> tshow expected <> "parameters. Got " <> tshow got <> "."
    display (CannotApplyNotAFunction nonfn applied) = "CannotApplyNotAFunction: value of type " <> display nonfn <> " is not a function. Cannot apply a value of type " <> display applied <> " to it."
    display (BadPartialApplication name) = "BadPartialApplication: attempted to flip the order of parameters for a value of type " <> display name <> ", but it does not take at least two parameters."
    display (UnexpectedValue got) = "UnexpectedValue: Unexpected value of type " <> display got <> "."

instance Display RuntimeException where
    display Boop100 = "RuntimeException"
    display Boop200 = "RuntimeException"

instance Display CompilationException where
    display (ParseErr e)    = display e
    display (TypeErr e)     = display e
