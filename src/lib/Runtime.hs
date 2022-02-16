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
    f [] = if fLen 0 then Right ([], []) else Left $ NoTokensToMatch name
    f tokens@(tok : _) = let tokens' = takeWhile (`Set.member` set) tokens
        in do
            x <- if not (fLen 0) && null tokens'
                 then Left $ BadMatch name tok
                 else if fLen $ length tokens'
                      then Right tokens'
                      else Left . TokenMiscount name $ length tokens'
            pure ([(name, x)], drop (length x) tokens)

parseTokens :: P -> [Text] -> Either ParseError [(Text, [Text])]
parseTokens (P f) tokens = case f tokens of
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
    = BadMatch Text Text
    | NoTokensToMatch Text
    | TokenMiscount Text Int
    | UnmatchedTokens (NonEmpty Text)
    deriving (Eq, Show, Read)

instance Display ParseError where
    display (BadMatch groupName token) = "The token group `" <> groupName <> "` didn't match enough tokens before encountering the foreign token `" <> token <> "`."
    display (NoTokensToMatch groupName) = "The token group `" <> groupName <> "` matches a non-zero number of tokens, but there are no tokens to match."
    display (TokenMiscount groupName matches) = "The token group `" <> groupName <> "` matched an invalid number of tokens. Matched: " <> tshow matches <> "."
    display (UnmatchedTokens (tok :| toks)) = tshow (length toks + 1) <> " unmatched tokens found starting at token: `" <> tok <> "`."
