module Schema.Types where

import           CustomPrelude -- import all


data Source
    = Source {
        filename :: Text
      , contents :: Text
    }
    deriving (Read, Show, Eq)

data Type
    = StringTag
    | CharTag
    | ListTag Type
    deriving (Read, Show, Eq)

data Expr'
    = StringU' Text
    | CharU' (Maybe Char)
    | ListU' [Expr']
    deriving (Read, Show, Eq)

data Expr
    = StringU Text
    | CharU Char
    | ListU [Expr]
    deriving (Read, Show, Eq)

data ExprT
    = String Text
    | Char Char
    | List Type [ExprT]
    deriving (Read, Show, Eq)