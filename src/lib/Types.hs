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
    | ApplyU ExprU ExprU
    deriving (Read, Show, Eq)

data ExprT
    = UnitT
    | StringT Text
    | IntT Int
    | BoolT Bool
    | ListT Type [ExprT]
    | FnDefT Name Type [Type]
    -- name of function, type of function, remaining param types, already applied values
    | FnT Name Type [ExprT]
    | ApplyT ExprT ExprT
    deriving (Read, Show, Eq)

data Type
    = UnitTag
    | StringTag
    | IntTag
    | BoolTag
    | ListTag Type
    -- param type, return type (e.g. \x -> (+x) is of type (FnTag Int (FnTag Int Int)))
    | FnTag Type Type
    deriving (Read, Show, Eq)

type Env = Map Name ExprT

type Closure = (Env, ExprT)


instance Display Type where
    display UnitTag = "unit"
    display StringTag = "string"
    display IntTag = "int"
    display BoolTag = "bool"
    display (ListTag t) = "List[" <> display t <> "]"
    -- note: fns with no params are just values.
    display (FnTag p ret) = display p <> " -> " <> display ret
