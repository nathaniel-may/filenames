module CodeGen where

import CustomPrelude -- import all
import Types         -- import all


gen :: ExprT -> Text
gen ast = header <> toHVal ast

-- TODO maybe codegen can operate on a post-opimized datatype that doesn't include some of these things.
toHVal :: ExprT -> Text
toHVal UnitT = "()"
toHVal (StringT s) = s
toHVal (IntT i) = tshow i -- TODO this is not right
toHVal (BoolT b) = tshow b -- TODO this is not right
toHVal (ListT _ xs) = tshow $ toHVal <$> xs
toHVal FnDefT{} = "" -- TODO stub
toHVal FnT{} = "" -- TODO stub
toHVal ApplyT{} = "" -- TODO stub

-- header just prints the value as the main operation
header :: Text
header = "main = print "
