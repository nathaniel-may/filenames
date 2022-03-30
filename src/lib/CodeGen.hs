module CodeGen where

import           CustomPrelude -- import all
import qualified Data.Text     as T
import           Types         -- import all


gen :: ExprT -> Text
gen ast = header <> "(" <> toHVal ast <> ")"

-- TODO maybe codegen can operate on a post-opimized datatype that doesn't include some of these things.
toHVal :: ExprT -> Text
toHVal UnitT = "()"
toHVal (StringT s) = "\"" <> s <> "\""
toHVal (IntT i) = tshow i
toHVal (BoolT b) = tshow b
toHVal (ListT _ xs) = "[" <> elems <> "]" where
    elems = T.dropEnd 1 (foldr (\x y -> x <> "," <> y) "" (toHVal <$> xs))
toHVal (FnT (Name "<") _ (x : y : _)) = "(" <> toHVal x <> " < " <> toHVal y <> ")"
toHVal (FnT (Name ">") _ (x : y : _)) = "(" <> toHVal x <> " > " <> toHVal y <> ")"
toHVal (FnT (Name "=")  _ (x : y : _)) = "(" <> toHVal x <> " == " <> toHVal y <> ")"
toHVal (FnT (Name "==") _ (x : y : _)) = "(" <> toHVal x <> " == " <> toHVal y <> ")"
toHVal (FnT (Name "<=") _ (x : y : _)) = "(" <> toHVal x <> " <= " <> toHVal y <> ")"
toHVal (FnT (Name ">=") _ (x : y : _)) = "(" <> toHVal x <> " >= " <> toHVal y <> ")"
toHVal (FnT name _ params) = error $ "BOOM: " <> display name <> " APPLIED TO" <> tshow params  -- Nothing should reach here if the typechecker is working. TODO should this have an error value? should I model builtins differently?
toHVal (ApplyT x y) = "(" <> toHVal x <> " " <> toHVal y <> ")"
toHVal (FlipT f) = "(flip " <> toHVal f <> ")"

-- header just prints the value as the main operation
header :: Text
header = "main = print "
