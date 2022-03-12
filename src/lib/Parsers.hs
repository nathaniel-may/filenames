module Parsers where

import CustomPrelude -- import all
import Exceptions   (ParseException(..))
import Types        (ExprU(..))

parse :: Text -> Either ParseException ExprU
parse _ = Left Boop10 -- TODO stub
