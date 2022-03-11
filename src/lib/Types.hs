{-# LANGUAGE GADTs #-}

module Types where

import CustomPrelude -- import all


data ExprU
    = StringU Text
    | IntU Int -- TODO nat?
    | ListU [ExprU]
    deriving (Read, Show, Eq)

data ExprT
    = StringT Text
    | IntT Int
    | BoolT Bool
    | ListT Type [ExprT]
    deriving (Read, Show, Eq)

data Type
    = StringTag
    | IntTag
    | BoolTag
    | ListTag Type
    deriving (Read, Show, Eq)

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
