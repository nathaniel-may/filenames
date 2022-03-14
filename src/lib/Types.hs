{-# LANGUAGE GADTs #-}

module Types where

import CustomPrelude -- import all


newtype Name
    = Name Text
    deriving (Read, Show, Eq, Ord)

data ExprU
    = RootU [ExprU]
    | StringU Text
    | IntU Int -- TODO nat?
    | BoolU Bool
    | ListU [ExprU]
    | AssignmentU Name ExprU
    | IdentifierU Name
    deriving (Read, Show, Eq)

data ExprT
    = UnitT
    | StringT Text
    | IntT Int
    | BoolT Bool
    | ListT Type [ExprT]
    deriving (Read, Show, Eq)

data Type
    = UnitTag
    | StringTag
    | IntTag
    | BoolTag
    | ListTag Type
    | UnknownTag -- TODO does this belong here?
    deriving (Read, Show, Eq)

type ValueTable = Map Name ExprT


instance Display Type where
    display UnitTag = "unit"
    display StringTag = "string"
    display IntTag = "int"
    display BoolTag = "bool"
    display (ListTag t) = "List[" <> display t <> "]"
    display UnknownTag = "unknown"

{-

d := "-"

format := with_delim d [
    no_delim (==1) ["art", "photo"]
  , delim d (>=0) ["nature", "architecture", "people"]
  , id
  , no_delim (>=0) ["1", "2", "3", "4",  "5", "6", "7", "8", "9"]
  ]

id := no_delim (==6) id_chars

// limited characters for example
id_chars := ["A", "B", "C", "D", "E", "F", "1", "2", "3"]

-}
