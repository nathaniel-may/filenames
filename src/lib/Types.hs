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
    | OpIdentifierU Name
    | FnCallU Name [ExprU]
    | LambdaU [ExprU] [ExprU]
    deriving (Read, Show, Eq)

data ExprT
    = UnitT
    | StringT Text
    | IntT Int
    | BoolT Bool
    | ListT Type [ExprT]
    | FnT Type [Type] [ExprT]
    | FnCallT Name Type [ExprT]
    deriving (Read, Show, Eq)

data Type
    = UnitTag
    | StringTag
    | IntTag
    | BoolTag
    | ListTag Type
    | FnTag Type [Type]
    deriving (Read, Show, Eq)

type ValueTable = Map Name ExprT

-- TODO
-- data BuiltIn
--     = BuiltIn Name ExprT
--     deriving (Read, Show, Eq)


instance Display Type where
    display UnitTag = "unit"
    display StringTag = "string"
    display IntTag = "int"
    display BoolTag = "bool"
    display (ListTag t) = "List[" <> display t <> "]"
    -- note: fns with no params are just values.
    display (FnTag ret []) = display ret
    display (FnTag ret params) = foldr (<>) "" symbols where
        symbols = interleave (display <$> params) (repeat " -> ") <> [display ret]
