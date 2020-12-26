module Exe where

import           CustomPrelude       hiding (some)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Options.Applicative -- import all
import           Parsers             hiding (Parser)
import           System.Directory    (listDirectory)
import           Text.Megaparsec     (errorBundlePretty, parse, ParseErrorBundle)

data Input = Input
  { details :: Bool 
  , schema  :: FilePath
  , dir     :: FilePath }

-- flag ast filenames
-- TODO replace with actual schemafile AST
data ParsedInput = ParsedInput Bool [Text] [Text]

-- flag valid invalid parseErrors
data Output = Output Bool [Text] [Text] [ParseErrorBundle Text Void]

-- definitions for input
input :: Parser Input
input = Input
      <$> switch
          ( long "details"
         <> short 'd'
         <> help "Whether to show individual parse errors" )
      <*> argument str
          ( metavar "SCHEMA-FILE"
         <> help "Schema file describing name structure" )
      <*> argument str
          ( metavar "DIR"
         <> help "Directory to check filenames" )

-- where the interaction with the user takes place
main :: IO ()
main = run =<< execParser opts
  where
    opts = info (input <**> helper)
      ( fullDesc
     <> progDesc "Parse filenames according to schema file"
     <> header ":::: filename parser ::::" )

run :: Input -> IO ()
run input' = interface . work =<< readInput input'

-- -- where file IO and parsing is done
-- run :: Input -> IO ()
-- run (Input dFlag s d) = do
--     putStrLn ""
--     filenames' <- listDirectory d
--     let filenames = T.pack <$> filenames'
--     schema' <- T.readFile s
--     let tags = T.lines schema'
--     let parsed = preserving (parse (filename tags) =<< T.unpack) <$> filenames
--     -- bind is filter, mempty :: Monoid b => a -> b
--     let pFails = bitraverse pure (either pure mempty) =<< parsed
--     let valid = length $ filter (isRight . snd) parsed
--     let invalid = length pFails
--     putStrLn $ tshow valid <> " valid filenames found."
--     putStrLn $ tshow invalid <> " invalid filenames found."
--     if null pFails
--     then putStrLn "Everything matches the schema!"
--     else if dFlag
--         then mapM_ (putStrLn . T.pack . errorBundlePretty) (snd <$> pFails)
--         else mapM_ putStrLn (fst <$> pFails)
--     putStrLn ""

readInput :: Input -> IO ParsedInput
readInput (Input dFlag s d) = do
    filenames <- fmap T.pack <$> listDirectory d
    tags <- T.lines <$> T.readFile s -- TODO replace with AST
    pure (ParsedInput dFlag tags filenames)

work :: ParsedInput -> Output
work (ParsedInput dFlag ast filenames) = Output dFlag valid invalid parserErrors where
    parsed = uncurry pairUp . preserving (parse (filename ast) =<< T.unpack) <$> filenames
    parserErrors = fmap snd . lefts $ parsed
    valid = fmap fst . rights $ parsed
    invalid = fmap fst . lefts $ parsed

interface :: Output -> IO ()
interface (Output dFlag valid invalid parseErrors) = do
    putStrLn ""
    putStrLn $ tshow (length valid) <> " valid filenames found."
    putStrLn $ tshow (length invalid) <> " invalid filenames found."
    putStrLn ""
    if null parseErrors
    then putStrLn "Everything matches the schema!"
    else if dFlag
        then mapM_ (putStrLn . T.pack . errorBundlePretty) parseErrors
        else mapM_ putStrLn invalid
    putStrLn ""

pairUp :: c -> Either a b -> Either (c, a) (c, b)
pairUp x (Left y) = Left (x, y)
pairUp x (Right z) = Right (x, z)