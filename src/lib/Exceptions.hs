module Exceptions where

import CustomPrelude -- import all

data TypeException
    = Boop1
    | Boop2
    deriving (Read, Show, Eq)

data RuntimeException
    = Boop100
    | Boop200
    deriving (Read, Show, Eq)
