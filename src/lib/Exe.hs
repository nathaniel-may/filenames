module Exe where

import           CustomPrelude       hiding (some)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Data.Bitraversable  (bitraverse)
import           Options.Applicative -- import all
import           Parsers             hiding (Parser, str)
import           System.Directory    (listDirectory)
import           Text.Megaparsec     (errorBundlePretty, parse)

data Input = Input
  { details :: Bool 
  , schema  :: FilePath
  , dir     :: FilePath }

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

-- where file IO and parsing is done
run :: Input -> IO ()
run (Input dFlag s d) = do
    putStrLn ""
    filenames' <- listDirectory d
    let filenames = T.pack <$> filenames'
    schema' <- T.readFile s
    let tags = T.lines schema'
    let parsed = preserving (parse (filename tags) =<< T.unpack) <$> filenames
    -- bind is filter, mempty :: Monoid b => a -> b
    let pFails = bitraverse pure (either pure mempty) =<< parsed
    let valid = length $ filter (isRight . snd) parsed
    let invalid = length pFails
    putStrLn $ tshow valid <> " valid filenames found."
    putStrLn $ tshow invalid <> " invalid filenames found."
    if null pFails
    then putStrLn "Everything matches the schema!"
    else if dFlag
        then mapM_ (putStrLn . T.pack . errorBundlePretty) (snd <$> pFails)
        else mapM_ putStrLn (fst <$> pFails)
    putStrLn ""