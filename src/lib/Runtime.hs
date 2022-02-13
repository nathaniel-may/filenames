module Runtime where

import           CustomPrelude  -- import all
import           Data.Semigroup (Semigroup)
import qualified Data.Set       as Set
import qualified Data.Text      as T


newtype P = P ([Text] -> Maybe ([(Text, [Text])], [Text]))

instance Semigroup P where
    (P f) <> (P g) = P $ \x -> do
        (fparsed, frest) <- f x
        (gparsed, grest) <- g frest
        pure (fparsed <> gparsed, grest)

p :: Text -> (Int -> Bool) -> Set Text -> P
p name fLen set = P f where
    f :: [Text] -> Maybe ([(Text, [Text])], [Text])
    f tokens = let tokens' = takeWhile (`Set.member` set) tokens
        in do
            x <- lengthMay fLen tokens'
            pure ([(name, x)], drop (length x) tokens)

lengthMay :: Foldable f => (Int -> Bool) -> f a -> Maybe (f a)
lengthMay f = g where
    g x = if f (length x) then Just x else Nothing

parseTokens :: P -> [Text] -> Maybe [(Text, [Text])]
parseTokens (P f) tokens = case f tokens of
    Nothing -> Nothing
    Just (namedTokens, []) -> Just namedTokens
    Just (_, _ : _) -> Nothing -- TODO Exception

parse :: P -> Char -> Text -> Maybe [(Text, [Text])]
parse parser delim input = parseTokens parser tokens where
    tokens = T.split (==delim) input -- "abc--123" would be ["abc", "", "123"]. TODO handle double delims
