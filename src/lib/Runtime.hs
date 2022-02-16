module Runtime where

import           CustomPrelude  -- import all
import           Data.Semigroup (Semigroup)
import qualified Data.Set       as Set
import qualified Data.Text      as T


newtype P = P ([Text] -> Either ParseError ([(Text, [Text])], [Text]))

instance Semigroup P where
    (P f) <> (P g) = P $ \x -> do
        (fparsed, frest) <- f x
        (gparsed, grest) <- g frest
        pure (fparsed <> gparsed, grest)

p :: Text -> (Int -> Bool) -> Set Text -> P
p name fLen set = P f where
    f :: [Text] -> Either ParseError ([(Text, [Text])], [Text])
    f tokens = let tokens' = takeWhile (`Set.member` set) tokens
        in do
            x <- maybeToRight (TokenMiscount name $ length tokens') (lengthMay fLen tokens')
            pure ([(name, x)], drop (length x) tokens)

lengthMay :: Foldable f => (Int -> Bool) -> f a -> Maybe (f a)
lengthMay f = g where
    g x = if f (length x) then Just x else Nothing

parseTokens :: P -> [Text] -> Either ParseError [(Text, [Text])]
parseTokens (P f) tokens = case f tokens of
    -- TODO make this a Left with more info on where it didn't match.
    Left e -> Left e
    Right (namedTokens, []) -> Right namedTokens
    Right (_, tok : toks) -> Left . UnmatchedTokens $ tok :| toks

parse :: P -> Char -> Text -> Either ParseError [(Text, [Text])]
parse parser delim input = parseTokens parser $ T.split (==delim) input

{-
This error type is part of expected runtime behavior, and
therefor isn't an exception.
-}
data ParseError
    = TokenMiscount Text Int
    | UnmatchedTokens (NonEmpty Text)
    deriving (Eq, Show, Read)

instance Display ParseError where
    display (TokenMiscount groupName matches) = "The token group `" <> groupName <> "` matched an invalid number of tokens: " <> tshow matches <> "."
    display (UnmatchedTokens (tok :| toks)) = tshow (length toks + 1) <> " unmatched tokens found starting at token: `" <> tok <> "`."
