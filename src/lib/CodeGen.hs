module CodeGen where

import CustomPrelude -- import all
import Types         -- import all


toHVal :: ExprT -> Text
toHVal (StringT s) = s
toHVal (IntT i) = tshow i
toHVal (BoolT b) = tshow b
toHVal (ListT _ xs) = tshow $ toHaskell <$> xs

-- header just prints the value as the main operation
header :: Text
header = "main = print "
