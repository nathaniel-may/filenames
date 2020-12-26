{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{- 
A prelude specfic to this project.
Should be paired with NoImplicitPrelude in build file.

Followed Stephen Diehl's post to create this: https://www.stephendiehl.com/posts/protolude.html
-}
module CustomPrelude (
      module CustomPrelude
    , module Export
    , P.FilePath
    , P.Read
) where

import qualified Prelude as P

-- Base types
import Data.Int    as Export
import Data.Bool   as Export
import Data.Char   as Export
import Data.Maybe  as Export
import Data.Either as Export
import Data.Void   as Export (Void)

import Data.Function as Export (
    ($)
  , id
  , const
  , (.)
  , flip
  , fix
  , on)

-- Base GHC types
import GHC.IO    as Export (IO)
import GHC.Num   as Export
import GHC.Real  as Export
import GHC.Float as Export
import GHC.Show  as Export

-- Debugging
import Debug.Trace as Debug

-- Safe
import Safe as Export (headMay, initMay, tailMay)

-- Applicative
import Control.Applicative as Export (
    Applicative(..)
  , Alternative(..)
  , Const(..)
  , ZipList(..)
  , (<**>)
  , liftA
  , liftA2
  , liftA3
  , optional)

-- Monad
import Control.Monad as Export

-- Base typeclasses
import GHC.Enum         as Export
import Data.Eq          as Export
import Data.Ord         as Export
import Data.Monoid      as Export
import Data.Traversable as Export
import Data.Foldable    as Export hiding (
    foldr1
  , foldl1
  , maximum
  , maximumBy
  , minimum
  , minimumBy)
import Data.Functor.Identity as Export
import Data.Functor          as Export (
    Functor(..)
  , ($>)
  , (<$>)
  , void)

-- Data structures
import Data.Tuple as Export
import Data.List as L
import Data.List  as Export (
    splitAt
  , break
  , intercalate
  , isPrefixOf
  , drop
  , dropWhile
  , filter
  , group
  , reverse
  , replicate
  , take
  , takeWhile)
-- only exporting types. functions should be imported qualified
import Data.Map      as Export (Map)
import Data.Set      as Export (Set)
import Data.Sequence as Export (Seq)
import Data.IntMap   as Export (IntMap)
import Data.IntSet   as Export (IntSet)

-- Text (should be imported qualified)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Data.Text    as Export (Text)
import Data.Text.IO as Export (putStrLn, putStr)

-- IO
import System.Exit as Export


-- * Basic functions

print :: Show a => a -> IO ()
print = P.putStrLn . show

identity :: a -> a
identity x = x

bind :: Monad m => (a -> m b) -> m a -> m b
bind = (=<<)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y = (\b -> if b then x else y) =<< p

tshow :: Show a => a -> Text
tshow = T.pack . show

preserving :: (a -> b) -> a -> (a, b)
preserving = ap (,)

(!!?) :: [a] -> Int -> Maybe a
(!!?) xs n
    | n < 0 = Nothing
    | otherwise = headMay $ L.drop n xs

count :: (a -> Bool) -> [a] -> Int
count f = foldr (\x n -> if f x then n + 1 else n) 0

none :: Foldable f => (a -> Bool) -> f a -> Bool
none f = not . any f

-- * Debugging

{-# WARNING undefined "'undefined' remains in code" #-}
undefined :: a
undefined = P.undefined

{-# WARNING error "'error' remains in code" #-}
error :: Text -> a
error = P.error . T.unpack

{-# WARNING trace "'trace' remains in code" #-}
trace :: Text -> a -> a
trace = Debug.trace . T.unpack

{-# WARNING traceShow "'traceShow' remains in code" #-}
traceShow :: P.Show a => a -> a
traceShow a = Debug.trace (P.show a) a

{-# WARNING traceM "'traceM' remains in code" #-}
traceM :: P.Monad m => P.String -> m ()
traceM = Debug.traceM

{-# WARNING traceIO "'traceIO' remains in code" #-}
traceIO :: P.String -> P.IO ()
traceIO = Debug.traceIO

-- * Either

leftToMaybe :: Either l r -> Maybe l
leftToMaybe = either Just (const Nothing)

rightToMaybe :: Either l r -> Maybe r
rightToMaybe = either (const Nothing) Just

maybeToRight :: l -> Maybe r -> Either l r
maybeToRight l = maybe (Left l) Right

maybeToLeft :: r -> Maybe l -> Either l r
maybeToLeft r = maybe (Right r) Left

maybeToEither :: Monoid b => (a -> b) -> Maybe a -> b
maybeToEither = maybe mempty