module CodeGen where

import           CustomPrelude -- import all
import qualified Data.Text     as T
import           Types         -- import all


gen :: ExprT -> Text
gen ast = header <> toHVal ast

-- TODO maybe codegen can operate on a post-opimized datatype that doesn't include some of these things.
toHVal :: ExprT -> Text
toHVal (StringT s) = "\"" <> s <> "\""
toHVal (AttributeT _) = "" -- TODO stub
toHVal (RecordT _ _) = "" -- TODO stub
toHVal (ListT _ xs) = "[" <> elems <> "]" where
    elems = T.dropEnd 1 (foldr (\x y -> x <> "," <> y) "" (toHVal <$> xs))
toHVal FnT{} = "" -- TODO stub

-- header just prints the value as the main operation
header :: Text
header = "main = print "
