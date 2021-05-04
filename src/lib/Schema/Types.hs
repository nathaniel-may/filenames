module Schema.Types where

import           CustomPrelude -- import all


data Type
    = StringTag
    | CharTag
    | ListTag Type
    deriving (Read, Show, Eq)

data Expr
    = StringU Text
    | CharU (Maybe Char)
    | ListU [Expr]
    deriving (Read, Show, Eq)

data ExprT
    = String Text
    | Char (Maybe Char)
    | List Type [ExprT]
    deriving (Read, Show, Eq)