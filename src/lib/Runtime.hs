module Runtime where

import           CustomPrelude  -- import all
import qualified Data.Set       as Set
import qualified Data.Text      as T


data TagGroup
    = TagGroup { name :: !Text
               , tags :: ![Text] }
    deriving (Eq, Ord, Show, Read)

newtype Filename 
    = Filename [TagGroup]
    deriving (Eq, Ord, Show, Read)

newtype Parser a
    = Parser { runParser :: [Text] -> Either ParseError (a, [Text]) }

instance Functor Parser where
    fmap f (Parser run) = Parser $ \x -> do
        (parsed, rest) <- run x
        pure (f parsed, rest)

instance Applicative Parser where
    pure x = Parser $ \rest -> Right (x, rest)
    liftA2 f (Parser runx) (Parser runy) = Parser $ \x -> do
        (y, restx) <- runx x
        (z, resty) <- runy restx
        pure (f y z, resty)

instance Monad Parser where
    (Parser run) >>= f = Parser $ \x -> do 
        (parsed, rest) <- run x
        let (Parser run2) = f parsed
        run2 rest

p :: Text -> (Int -> Bool) -> Set Text -> Parser TagGroup
p n fLen set = Parser f where
    f :: [Text] -> Either ParseError (TagGroup, [Text])
    f [] = if fLen 0 then Right (TagGroup n [], []) else Left $ NoTokensToMatch n
    f tokens@(tok : _) = let tokens' = takeWhile (`Set.member` set) tokens
        in do
            x <- if not (fLen 0) && null tokens'
                 then Left $ BadMatch n tok
                 else if fLen $ length tokens'
                      then Right tokens'
                      else Left . TokenMiscount n $ length tokens'
            pure (TagGroup n x, drop (length x) tokens)

parseTokens :: Parser Filename -> [Text] -> Either ParseError Filename
parseTokens (Parser f) tokens = case f tokens of
    Left e -> Left e
    Right (namedTokens, []) -> Right namedTokens
    Right (_, tok : toks) -> Left . UnmatchedTokens $ tok :| toks

parse :: Parser Filename -> Char -> Text -> Either ParseError Filename
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
