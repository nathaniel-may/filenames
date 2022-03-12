module CodeGen where

import CustomPrelude -- import all
import Types         -- import all


gen :: ExprT -> Text
gen ast = header <> toHVal ast

toHVal :: ExprT -> Text
toHVal (StringT s) = s
toHVal (IntT i) = tshow i -- TODO this is not right
toHVal (BoolT b) = tshow b -- TODO this is not right
toHVal (ListT _ xs) = tshow $ toHVal <$> xs

-- header just prints the value as the main operation
header :: Text
header = "main = print "
