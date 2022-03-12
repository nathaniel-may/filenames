module Exceptions where

import CustomPrelude -- import all

data ParseException
    = Boop10
    | Boop20

data TypeException
    = Boop1
    | Boop2
    deriving (Read, Show, Eq)

data RuntimeException
    = Boop100
    | Boop200
    deriving (Read, Show, Eq)

data CompilationException
    = ParseErr ParseException
    | TypeErr TypeException
    | RuntimeErr RuntimeException

instance Display ParseException where
    display Boop10 = "ParseException"
    display Boop20 = "ParseException"

instance Display TypeException where
    display Boop1 = "TypeException"
    display Boop2 = "TypeException"

instance Display RuntimeException where
    display Boop100 = "RuntimeException"
    display Boop200 = "RuntimeException"

instance Display CompilationException where
    display (ParseErr e)    = display e
    display (TypeErr e)     = display e
    display (RuntimeErr e)  = display e
