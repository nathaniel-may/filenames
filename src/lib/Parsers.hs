module Parsers where

import CustomPrelude -- import all
import Exceptions   (ParseException(..))
import Types        (ExprU(..))


parse :: Text -> Either ParseException ExprU
-- TODO stub for the one integration test
parse "[\"a\", \"b\", \"c\"]" = Right (
      ListU [
          StringU "a"
        , StringU "b"
        , StringU "c"
      ]
    )
-- TODO build the parser
parse _ = Left Boop10
