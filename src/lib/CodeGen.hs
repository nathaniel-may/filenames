module CodeGen where

import           CustomPrelude -- import all
import qualified Data.Text     as T
import           Types         -- import all


gen :: ExprT -> Text
gen ast = header <> toHVal ast

-- TODO maybe codegen can operate on a post-opimized datatype that doesn't include some of these things.
toHVal :: ExprT -> Text
toHVal UnitT = "()"
toHVal (StringT s) = "\"" <> s <> "\""
toHVal (IntT i) = tshow i
toHVal (BoolT b) = tshow b
toHVal (ListT _ xs) = "[" <> elems <> "]" where
    elems = T.dropEnd 1 (foldr (\x y -> x <> "," <> y) "" (toHVal <$> xs))
toHVal FnDefT{} = "" -- TODO stub
toHVal FnT{} = "" -- TODO stub
toHVal ApplyT{} = "" -- TODO stub

-- header just prints the value as the main operation
header :: Text
header = "main = print "
