{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Types where

import           CustomPrelude -- import all
import qualified Data.Text     as T


newtype Name
    = Name Text
    deriving (Read, Show, Eq, Ord)

data Attribute
    = Unique | Optional
    deriving (Read, Show, Eq)

data ExprU
    = StringU Text
    | ListU [ExprU]
    | IdentifierU Name
    | ApplyU ExprU ExprU
    deriving (Read, Show, Eq)

data ExprT
    = StringT Text
    | AttributeT Attribute
    | ListT Type [ExprT]
    | RecordT Name [(Name, ExprT)]
    | FnT Name Type [ExprT]
    deriving (Read, Show, Eq)

data Type
    = StringTag
    | AttributeTag
    | ListTag Type
    | RecordTag Name [(Name, Type)]
    | FnTag Type Type
    deriving (Read, Show, Eq)

type Env = Map Name ExprT

type Closure = (Env, ExprT)

instance Display Type where
    display StringTag = "string"
    display AttributeTag = "attribute"
    display (ListTag t) = "List[" <> display t <> "]"
    display (RecordTag name _) = display name
    display (FnTag param ret) = display param <> " -> " <> display ret

instance Display Name where
    display (Name name) = name

instance Display a => Display [a] where
    display xs = "[" <> elems <> "]" where
        elems = T.dropEnd 1 (foldr (\x y -> x <> "," <> y) "" (display <$> xs))
