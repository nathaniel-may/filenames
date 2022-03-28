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
    | MultipleUnassignedValuesInBody [Type]
    | FunctionHasNoBody Name Type
    | FormatNotFound
    | ListTypeMismatch Type Type
    | NoValueNamed Name
    | NoFunctionNamed Name
    | NotAFunction Name Type
    | TypeMismatch Type Type
    | TopLevelNotAssignment -- TODO reference source at error (getting the type here is awkward work)
    | TypeMismatchFnReturn Name Type Type
    | TypeMismatchFnParam Name Type Type
    | TypeMismatchNumFnParams Name Int Int
    | CannotApplyNotAFunction Type Type
    | BadPartialApplication Type
    | MultipleAssignmentsWithName Name
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
    display (MultipleUnassignedValuesInBody types) = "MultipleUnassignedValuesInBody: unexpected unassigned values of the following types: " <> display types
    display (FunctionHasNoBody name got) = "FunctionHasNoBody: expected function " <> display name <> " to have a body. Instead found: " <> display got <> "."
    display FormatNotFound = "FormatNotFound: No value named 'format' found. 'format' is a required value."
    display (ListTypeMismatch expected got) = "ListTypeMismatch: List of type " <> display (ListTag expected) <> " but got " <> display (ListTag got) <> "." 
    display (NoValueNamed name) = "NoValueNamed: No value named '" <> display name <> "'."
    display (NoFunctionNamed name) = "NoFunctionNamed: No value named '" <> display name <> "'."
    display (TypeMismatch expected got) = "TypeMismatch: Expected type " <> display expected <> " but got type " <> display got <> "."
    display TopLevelNotAssignment = "TopLevelNotAssignment: All top-level expressions must be assignments."
    display (TypeMismatchFnReturn name expected got) = "TypeMismatchFnReturn: Expected type " <> display expected <> ", but function '" <> display name <> "' returns type " <> display got <> "."
    display (TypeMismatchFnParam name expected got) = "TypeMismatchFnParam: The function " <> display name <> " expected parameter of type " <> display expected <> ", but got type " <> display got <> "."
    display (TypeMismatchNumFnParams name expected got) = "TypeMismatchNumFnParams: Function '" <> display name <> "' requires " <> tshow expected <> "parameters. Got " <> tshow got <> "."
    display (NotAFunction name got) = "NotAFunction: Expected value " <> display name <> " to be a function. Instead it's defined as a " <> display got <> "."
    display (CannotApplyNotAFunction nonfn applied) = "CannotApplyNotAFunction: value of type " <> display nonfn <> " is not a function. Cannot apply a value of type " <> display applied <> " to it."
    display (BadPartialApplication name) = "BadPartialApplication: attempted to flip the order of parameters for a value of type " <> display name <> ", but it does not take at least two parameters."
    display (MultipleAssignmentsWithName name) = "MultipleAssignmentsWithName: There already exists a value with the name " <> display name <> "."

instance Display RuntimeException where
    display Boop100 = "RuntimeException"
    display Boop200 = "RuntimeException"

instance Display CompilationException where
    display (ParseErr e)    = display e
    display (TypeErr e)     = display e
