{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{- 
A prelude specfic to this project.
Should be paired with NoImplicitPrelude in build file.

Doesn't re-export Text so the use sites can use a qualified import.
-}
module CustomPrelude (
      module CustomPrelude
    , module Export
    , putStrLn, Text
    , Void
    , FilePath, IO
    , Read
    , Show, show
) where

import           Control.Applicative as Export
import           Control.Monad       as Export
import           Data.Bool           as Export
import           Data.Char           as Export
import           Data.Either         as Export
import           Data.Eq             as Export
import           Data.Eq             (Eq)
import           Data.Foldable       as Export
import           Data.Function       as Export
import           Data.Int            as Export
import           Data.List           as Export
import           Data.Maybe          as Export
import           Data.Monoid         as Export
import qualified Data.Text           as T
import           Data.Text           (Text)
import           Data.Text.IO        (putStrLn)
import           Data.Traversable    as Export
import           Data.Tuple          as Export
import           Data.Void           (Void)
import           GHC.Num             as Export
import           System.IO           (FilePath, IO)
import           Text.Read           (Read)
import           Text.Show           (Show, show)

tshow :: Show a => a -> Text
tshow = T.pack . show

preserving :: (a -> b) -> a -> (a, b)
preserving = ap (,)

bind :: Monad m => (a -> m b) -> m a -> m b
bind = (=<<)