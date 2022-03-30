module CodeGen where

import           CustomPrelude -- import all
import qualified Data.Text     as T
import           Types         -- import all


gen :: ExprO -> Text
gen ast = header <> "(" <> toHVal ast <> ")"

-- TODO maybe codegen can operate on a post-opimized datatype that doesn't include some of these things.
toHVal :: ExprO -> Text
toHVal UnitO = "()"
toHVal (StringO s) = "\"" <> s <> "\""
toHVal (IntO x) = tshow x
toHVal (BoolO x) = tshow x
toHVal (ListO xs) = "[" <> elems <> "]" where
    elems = T.dropEnd 1 (foldr (\x y -> x <> "," <> y) "" (toHVal <$> xs))
toHVal (FnO (Name "<") (x : y : _)) = "(" <> toHVal x <> " < " <> toHVal y <> ")"
toHVal (FnO (Name ">") (x : y : _)) = "(" <> toHVal x <> " > " <> toHVal y <> ")"
toHVal (FnO (Name "=")  (x : y : _)) = "(" <> toHVal x <> " == " <> toHVal y <> ")"
toHVal (FnO (Name "==") (x : y : _)) = "(" <> toHVal x <> " == " <> toHVal y <> ")"
toHVal (FnO (Name "<=") (x : y : _)) = "(" <> toHVal x <> " <= " <> toHVal y <> ")"
toHVal (FnO (Name ">=") (x : y : _)) = "(" <> toHVal x <> " >= " <> toHVal y <> ")"
toHVal FnO{} = toHVal UnitO -- Nothing should reach here if the typechecker is working. TODO should this have an error value? should I model builtins differently?

-- header just prints the value as the main operation
header :: Text
header = "main = print "
